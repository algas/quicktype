import { mapContains } from "collection-utils";

import { TargetLanguage } from "../TargetLanguage";
import { EnumOption, StringOption, BooleanOption, Option, getOptionValues, OptionValues } from "../RendererOptions";
import { Type, ClassType, UnionType, EnumType, ClassProperty } from "../Type";
import { matchType, nullableFromUnion } from "../TypeUtils";
import { ConvenienceRenderer, ForbiddenWordsInfo } from "../ConvenienceRenderer";
import { Namer, Name, DependencyName, funPrefixNamer } from "../Naming";
import {
    legalizeCharacters,
    isLetterOrUnderscoreOrDigit,
    isLetterOrUnderscore,
    isAscii,
    splitIntoWords,
    combineWords,
    firstUpperWordStyle,
    allLowerWordStyle,
    allUpperWordStyle,
} from "../support/Strings";
import { Sourcelike, annotated, MultiWord, singleWord, multiWord, parenIfNeeded } from "../Source";
import { anyTypeIssueAnnotation } from "../Annotation";
import { RenderContext } from "../Renderer";

export const haskellOptions = {
    justTypes: new BooleanOption("just-types", "Plain types only", false),
    useList: new EnumOption("array-type", "Use Array or List", [
        ["array", false],
        ["list", true],
    ]),
    // FIXME: Do this via a configurable named eventually.
    moduleName: new StringOption("module", "Generated module name", "NAME", "QuickType"),
};

export class HaskellTargetLanguage extends TargetLanguage {
    constructor() {
        super("Haskell", ["haskell"], "haskell");
    }

    protected getOptions(): Option<any>[] {
        return [haskellOptions.justTypes, haskellOptions.moduleName, haskellOptions.useList];
    }

    get supportsOptionalClassProperties(): boolean {
        return true;
    }

    get supportsUnionsWithBothNumberTypes(): boolean {
        return true;
    }

    protected makeRenderer(
        renderContext: RenderContext,
        untypedOptionValues: { [name: string]: any }
    ): HaskellRenderer {
        return new HaskellRenderer(this, renderContext, getOptionValues(haskellOptions, untypedOptionValues));
    }
}

const forbiddenNames = [
    // reserved keywords
    "as",
    "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "family",
    "forall",
    "foreign",
    "hiding",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "of",
    "mdo",
    "module",
    "newtype",
    "proc",
    "qualified",
    "rec",
    "then",
    "type",
    "where",
    // in Prelude keywords ...
    "Array",
    "HashMap",
    "Map",
    "Maybe",
    "Bool",
    "Int",
    "True",
    "False",
    // Aeson types
    "encode",
    "decode",
];

const legalizeName = legalizeCharacters((cp) => isAscii(cp) && isLetterOrUnderscoreOrDigit(cp));

function haskellNameStyle(original: string, upper: boolean): string {
    const words = splitIntoWords(original);
    return combineWords(
        words,
        legalizeName,
        upper ? firstUpperWordStyle : allLowerWordStyle,
        firstUpperWordStyle,
        upper ? allUpperWordStyle : allLowerWordStyle,
        allUpperWordStyle,
        "",
        isLetterOrUnderscore
    );
}

const upperNamingFunction = funPrefixNamer("upper", (n) => haskellNameStyle(n, true));
const lowerNamingFunction = funPrefixNamer("lower", (n) => haskellNameStyle(n, false));

export class HaskellRenderer extends ConvenienceRenderer {
    constructor(
        targetLanguage: TargetLanguage,
        renderContext: RenderContext,
        private readonly _options: OptionValues<typeof haskellOptions>
    ) {
        super(targetLanguage, renderContext);
    }

    protected forbiddenNamesForGlobalNamespace(): string[] {
        return forbiddenNames;
    }

    protected makeTopLevelDependencyNames(t: Type, topLevelName: Name): DependencyName[] {
        t;
        topLevelName;
        return [];
    }

    protected makeNamedTypeNamer(): Namer {
        return upperNamingFunction;
    }

    protected makeNamedTypeDependencyNames(_: Type, typeName: Name): DependencyName[] {
        _;
        typeName;
        return [];
    }

    protected namerForObjectProperty(): Namer {
        return lowerNamingFunction;
    }

    protected forbiddenForObjectProperties(_c: ClassType, _className: Name): ForbiddenWordsInfo {
        return { names: [], includeGlobalForbidden: true };
    }

    protected makeUnionMemberNamer(): Namer {
        return upperNamingFunction;
    }

    protected get unionMembersInGlobalNamespace(): boolean {
        return true;
    }

    protected makeEnumCaseNamer(): Namer {
        return upperNamingFunction;
    }

    protected get enumCasesInGlobalNamespace(): boolean {
        return true;
    }

    protected proposeUnionMemberName(
        u: UnionType,
        unionName: Name,
        fieldType: Type,
        lookup: (n: Name) => string
    ): string {
        const fieldName = super.proposeUnionMemberName(u, unionName, fieldType, lookup);
        return `${fieldName}_in_${lookup(unionName)}`;
    }

    protected get commentLineStart(): string {
        return "-- ";
    }

    protected emitDescriptionBlock(lines: Sourcelike[]): void {
        if (lines.length === 1) {
            this.emitLine("{-| ", lines[0], " -}");
        } else {
            this.emitCommentLines(lines, "", undefined, "-}", "{-| ");
        }
    }

    private haskellType(t: Type, noOptional: boolean = false): MultiWord {
        return matchType<MultiWord>(
            t,
            (_anyType) => singleWord(annotated(anyTypeIssueAnnotation, "Jdec.Value")),
            (_nullType) => singleWord("()"),
            (_boolType) => singleWord("Bool"),
            (_integerType) => singleWord("Int"),
            (_doubleType) => singleWord("Float"),
            (_stringType) => singleWord("Text"),
            // (arrayType) => multiWord(" ", this.arrayType, parenIfNeeded(this.haskellType(arrayType.items))),
            (arrayType) => {
                if (this._options.useList) {
                    return multiWord("", "[", parenIfNeeded(this.haskellType(arrayType.items)), "]");
                }
                return multiWord(" ", "Vector", parenIfNeeded(this.haskellType(arrayType.items)));
            },
            (classType) => singleWord(this.nameForNamedType(classType)),
            (mapType) => multiWord(" ", "HashMap Text", parenIfNeeded(this.haskellType(mapType.values))),
            (enumType) => singleWord(this.nameForNamedType(enumType)),
            (unionType) => {
                const nullable = nullableFromUnion(unionType);
                if (nullable !== null) {
                    const nullableType = this.haskellType(nullable);
                    if (noOptional) return nullableType;
                    return multiWord(" ", "Maybe", parenIfNeeded(nullableType));
                }
                return singleWord(this.nameForNamedType(unionType));
            }
        );
    }

    private haskellProperty(p: ClassProperty): Sourcelike {
        if (p.isOptional) {
            return multiWord(" ", "Maybe", parenIfNeeded(this.haskellType(p.type, true))).source;
        } else {
            return this.haskellType(p.type).source;
        }
    }

    private emitClassDefinition(c: ClassType, className: Name): void {
        let description = this.descriptionForType(c);
        this.forEachClassProperty(c, "none", (name, jsonName) => {
            const propertyDescription = this.descriptionForClassProperty(c, jsonName);
            if (propertyDescription === undefined) return;

            if (description === undefined) {
                description = [];
            } else {
                description.push("");
            }
            description.push(`${this.sourcelikeToString(name)}:`);
            description.push(...propertyDescription);
        });

        this.emitDescription(description);
        this.emitLine("data ", className, " = ", className);
        this.indent(() => {
            let onFirst = true;
            this.forEachClassProperty(c, "none", (name, _jsonName, p) => {
                this.emitLine(onFirst ? "{" : ",", " _", className, "_", name, " :: ", this.haskellProperty(p));
                onFirst = false;
            });
            if (onFirst) {
                this.emitLine("{");
            }
            this.emitLine("} deriving (Generic, Show)");
        });
        this.emitEncoderInstances(className);
    }

    private emitEnumDefinition(e: EnumType, enumName: Name): void {
        this.emitDescription(this.descriptionForType(e));
        this.emitLine("data ", enumName);
        this.indent(() => {
            let onFirst = true;
            this.forEachEnumCase(e, "none", (name) => {
                const equalsOrPipe = onFirst ? "=" : "|";
                this.emitLine(equalsOrPipe, " ", name);
                onFirst = false;
            });
        });
        this.emitEncoderInstances(enumName);
    }

    private emitUnionDefinition(u: UnionType, unionName: Name): void {
        this.emitDescription(this.descriptionForType(u));
        this.emitLine("data ", unionName);
        this.indent(() => {
            let onFirst = true;
            this.forEachUnionMember(u, null, "none", null, (constructor, t) => {
                const equalsOrPipe = onFirst ? "=" : "|";
                if (t.kind === "null") {
                    this.emitLine(equalsOrPipe, " ", constructor);
                } else {
                    this.emitLine(equalsOrPipe, " ", constructor, " ", parenIfNeeded(this.haskellType(t)));
                }
                onFirst = false;
            });
        });
        this.emitEncoderInstances(unionName);
    }

    private emitEncoderInstances(className: Name): void {
        this.emitLine("makeFields ''", className);
        this.emitLine("deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop (2 + length (\"", className, "\" :: String))} ''", className);
    }

    private emitLanguageExtensions(ext: string): void {
        this.emitLine(`{-# LANGUAGE ${ext} #-}`);
    }

    protected emitSourceStructure(): void {
        const exports: Sourcelike[] = [];
        this.forEachTopLevel("none", (_, name) => {
            exports.push([name, " (..)"]);
        });
        this.forEachObject("none", (t: ClassType, name: Name) => {
            if (!mapContains(this.topLevels, t)) exports.push([name, " (..)"]);
        });
        this.forEachEnum("none", (t, name) => {
            if (!mapContains(this.topLevels, t)) exports.push([name, " (..)"]);
        });
        this.forEachUnion("none", (t, name) => {
            if (!mapContains(this.topLevels, t)) exports.push([name, " (..)"]);
        });

        this.emitLanguageExtensions('DeriveAnyClass');
        this.emitLanguageExtensions('DeriveGeneric');
        this.emitLanguageExtensions('FunctionalDependencies');
        this.emitLanguageExtensions('OverloadedStrings');
        this.emitLanguageExtensions('TemplateHaskell');

        if (!this._options.justTypes) {
            this.ensureBlankLine();
            this.emitLine("module ", this._options.moduleName);
            this.indent(() => {
                for (let i = 0; i < exports.length; i++) {
                    this.emitLine(i === 0 ? "(" : ",", " ", exports[i]);
                }
                this.emitLine(") where");
            });
            this.ensureBlankLine();
            this.emitMultiline(`import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics`);
            if (this._options.useList) {
                // this.emitLine("import List (map)");
            } else {
                this.emitLine("import Data.Vector (Vector)");
            }
        }

        this.forEachNamedType(
            "leading-and-interposing",
            (c: ClassType, className: Name) => this.emitClassDefinition(c, className),
            (e: EnumType, enumName: Name) => this.emitEnumDefinition(e, enumName),
            (u: UnionType, unionName: Name) => this.emitUnionDefinition(u, unionName)
        );

        if (this._options.justTypes) return;

        this.ensureBlankLine();
    }
}
