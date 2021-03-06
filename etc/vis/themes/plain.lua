-- Plain eight-color scheme
local lexers = vis.lexers

lexers.STYLE_DEFAULT ='back:default,fore:default'
lexers.STYLE_NOTHING = 'back:default'
lexers.STYLE_CLASS = 'fore:yellow'
lexers.STYLE_COMMENT = 'fore:blue'
lexers.STYLE_CONSTANT = 'fore:cyan'
lexers.STYLE_DEFINITION = 'fore:blue'
lexers.STYLE_ERROR = 'fore:red,italics'
lexers.STYLE_FUNCTION = 'fore:blue'
lexers.STYLE_KEYWORD = 'fore:yellow'
lexers.STYLE_LABEL = 'fore:green'
lexers.STYLE_NUMBER = 'fore:red'
lexers.STYLE_OPERATOR = 'fore:cyan'
lexers.STYLE_REGEX = 'fore:green'
lexers.STYLE_STRING = 'fore:red'
lexers.STYLE_PREPROCESSOR = 'fore:magenta'
lexers.STYLE_TAG = 'fore:red'
lexers.STYLE_TYPE = 'fore:green'
lexers.STYLE_VARIABLE = 'fore:blue'
lexers.STYLE_WHITESPACE = ''
lexers.STYLE_EMBEDDED = 'back:blue'
lexers.STYLE_IDENTIFIER = 'fore:default'

lexers.STYLE_LINENUMBER = 'fore:default'
lexers.STYLE_LINENUMBER_CURSOR = lexers.STYLE_LINENUMBER
lexers.STYLE_CURSOR = 'reverse'
lexers.STYLE_CURSOR_PRIMARY = lexers.STYLE_CURSOR..',fore:white'
lexers.STYLE_CURSOR_LINE = 'underlined'
lexers.STYLE_COLOR_COLUMN = 'back:yellow'
lexers.STYLE_SELECTION = 'back:blue'
lexers.STYLE_STATUS = 'reverse'
lexers.STYLE_STATUS_FOCUSED = 'reverse'
lexers.STYLE_SEPARATOR = lexers.STYLE_DEFAULT
lexers.STYLE_INFO = 'fore:default,back:default'
lexers.STYLE_EOF = ''
