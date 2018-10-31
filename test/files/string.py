DATETIME_FORMAT = r'j. E Y \k\e\l\l\o G.i'

# This string contains \r followed by \n
"""
"""

latex_preamble = r'''
\usepackage{amsmath}
\DeclareUnicodeCharacter{00A0}{\nobreakspace}
% In the parameters section, place a newline after the Parameters
% header
\usepackage{expdlist}
\let\latexdescription=\description
\def\description{\latexdescription{}{} \breaklabel}
% Make Examples/etc section headers smaller and more compact
\makeatletter
\titleformat{\paragraph}{\normalsize\py@HeaderFamily}%
            {\py@TitleColor}{0em}{\py@TitleColor}{\py@NormalColor}
\titlespacing*{\paragraph}{0pt}{1ex}{0pt}
\makeatother
% Fix footer/header
\renewcommand{\chaptermark}[1]{\markboth{\MakeUppercase{\thechapter.\ #1}}{}}
\renewcommand{\sectionmark}[1]{\markright{\MakeUppercase{\thesection.\ #1}}}
'''

def literal(value, type_=None):
    r"""Return a literal clause, bound to a bind parameter.

    Literal clauses are created automatically when non-
    :class:`.ClauseElement` objects (such as strings, ints, dates, etc.) are
    used in a comparison operation with a :class:`.ColumnElement` subclass,
    such as a :class:`~sqlalchemy.schema.Column` object.  Use this function
    to force the generation of a literal clause, which will be created as a
    :class:`BindParameter` with a bound value.

    :param value: the value to be bound. Can be any Python object supported by
        the underlying DB-API, or is translatable via the given type argument.

    :param type\_: an optional :class:`~sqlalchemy.types.TypeEngine` which
        will provide bind-parameter translation for this literal.

    """
    return BindParameter(None, value, type_=type_, unique=True)
