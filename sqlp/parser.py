# The contents of this file are subject to the Nymgo License, version 1.0.
# It is forbidden the use of the codes included on this file, even partially,
# for third parties, such as its copy, without express authorization of the
# intelectual property and commercial rights owners. A copy of the license
# may be obtained at http://nymgo.com/source-license. The intelectual
# property and comercial rights of this file and codes belong to Nymgo S.A.
#
# Copyright (c) 2014. All rights reserved.

"""A parser for SQL statements."""

import re
import pprint
import os, sys, re
from ply import lex, yacc


def after(l, start):
  s = start[:]
  x = l[:]
  s.reverse()
  x.reverse()
  count = 0
  while len(x) > 0 and s[0] != x[0]:
    count += 1
    del x[0]
  if len(x) == 0:
    return -1
  del s[0]
  while len(x) > 0 and len(s) > 0:
    if s[0] == x[0]:
      del s[0]
    del x[0]
  if len(s) > 0:
    return -1
  return count

class Location(dict):
  _line = None

  def __init__(self, lexer, lineno, lexpos):
    dict.__init__(self)
    self['lineno'] = lineno
    self._lexpos = lexpos
    self._lexdata = lexer.lexdata
    self['file'] = getattr(lexer, 'filename', "<unknown>")

  def __eq__(self, other):
    return self._lexpos == other._lexpos and \
         self['file'] == other['file']

  def resolve(self):
    if self._line:
      return

    startofline = self._lexdata.rfind('\n', 0, self._lexpos) + 1
    endofline = self._lexdata.find('\n', self._lexpos)
    self._line = self._lexdata[startofline:endofline]
    self._colno = self._lexpos - startofline

  def pointerline(self):
    def i():
      for i in xrange(0, self._colno):
        yield " "
      yield "^"

    return "".join(i())

  def __str__(self):
    self.resolve()
    return "%s line %s:%s\n%s\n%s" % (self['file'],
                      self['lineno']==0 and 1 or self['lineno'],
                      self._colno==0 and 1 or self._colno,
                      self._line, self.pointerline())

class SQLError(Exception):
  def __init__(self, message, location, warning=False):
    self.message = message
    self.location = location
    self.warning = warning

  def __str__(self):
    return "%s: %s, %s" % (self.warning and 'warning' or 'error',
                           self.message, self.location)

class SQLParser(object):
  # lex section ------------------------------------------------------------

  # Reserved words
  reserved = {
    'add':'ADD',
    'after':'AFTER',
    'all':'ALL',
    'alter':'ALTER',
    'any':'ANY',
    'as':'AS',
    'asc':'ASC',
    'auto_increment':'AUTO_INCREMENT',
    'avg':'AVG',
    'before':'BEFORE',
    'begin':'BEGIN',
    'between':'BETWEEN',
    'bigint':'BIGINT',
    'bit_and':'BIT_AND',
    'bit_or':'BIT_OR',
    'bit_xor':'BIT_XOR',
    'blob':'BLOB',
    'by':'BY',
    'cascade':'CASCADE',
    'case':'CASE',
    'cast':'CAST',
    'chain':'CHAIN',
    'change':'CHANGE',
    'character':'CHARACTER',
    'char':'CHAR',
    'charset':'CHARSET',
    'close':'CLOSE',
    'collate':'COLLATE',
    'column':'COLUMN',
    'commit':'COMMIT',
    'consistent':'CONSISTENT',
    'constraint':'CONSTRAINT',
    'convert':'CONVERT',
    'count':'COUNT',
    'create':'CREATE',
    'cross':'CROSS',
    'cube':'CUBE',
    'date':'DATE',
    'datetime':'DATETIME',
    'decimal':'DECIMAL',
    'declare':'DECLARE',
    'default':'DEFAULT',
    'delete':'DELETE',
    'desc':'DESC',
    'distinct':'DISTINCT',
    'do':'DO',
    'drop':'DROP',
    'dual':'DUAL',
    'each':'EACH',
    'else':'ELSE',
    'elseif':'ELSEIF',
    'end':'END',
    'engine':'ENGINE',
    'escape':'ESCAPE',
    'exists':'EXISTS',
    'false':'FALSE',
    'fetch':'FETCH',
    'first':'FIRST',
    'float':'FLOAT',
    'force':'FORCE',
    'for':'FOR',
    'from':'FROM',
    'from':'FROM',
    'global':'GLOBAL',
    'group':'GROUP',
    'having':'HAVING',
    'if':'IF',
    'ignore':'IGNORE',
    'index':'INDEX',
    'in':'IN',
    'inner':'INNER',
    'insert':'INSERT',
    'integer':'INTEGER',
    'int':'INT',
    'into':'INTO',
    'is':'IS',
    'iterate':'ITERATE',
    'join':'JOIN',
    'key':'KEY',
    'leave':'LEAVE',
    'left':'LEFT',
    'like':'LIKE',
    'limit':'LIMIT',
    'longblob':'LONGBLOB',
    'longtext':'LONGTEXT',
    'loop':'LOOP',
    'low_priority':'LOW_PRIORITY',
    'max':'MAX',
    'mediumblob':'MEDIUMBLOB',
    'mediumint':'MEDIUMINT',
    'mediumtext':'MEDIUMTEXT',
    'min':'MIN',
    'modify':'MODIFY',
    'natural':'NATURAL',
    'next':'NEXT',
    'no':'NO',
    'not':'NOT',
    'null':'NULL',
    'numeric':'NUMERIC',
    'offset':'OFFSET',
    'on':'ON',
    'open':'OPEN',
    'optimize':'OPTIMIZE',
    'order':'ORDER',
    'outer':'OUTER',
    'outer':'OUTER',
    'primary':'PRIMARY',
    'quick':'QUICK',
    'regexp':'REGEXP',
    'release':'RELEASE',
    'rename':'RENAME',
    'repeat':'REPEAT',
    'restrict':'RESTRICT',
    'return':'RETURN',
    'right':'RIGHT',
    'rollback':'ROLLBACK',
    'rollup':'ROLLUP',
    'row':'ROW',
    'select':'SELECT',
    'session':'SESSION',
    'set':'SET',
    'signed':'SIGNED',
    'smallint':'SMALLINT',
    'snapshot':'SNAPSHOT',
    'some':'SOME',
    'sounds':'SOUNDS',
    'start':'START',
    'sum':'SUM',
    'table':'TABLE',
    'temporary':'TEMPORARY',
    'text':'TEXT',
    'then':'THEN',
    'timestamp':'TIMESTAMP',
    'time':'TIME',
    'tinyblob':'TINYBLOB',
    'tinyint':'TINYINT',
    'tinytext':'TINYTEXT',
    'to':'TO',
    'transaction':'TRANSACTION',
    'trigger':'TRIGGER',
    'true':'TRUE',
    'unique':'UNIQUE',
    'unknown':'UNKNOWN',
    'unsigned':'UNSIGNED',
    'until':'UNTIL',
    'update':'UPDATE',
    'use':'USE',
    'using':'USING',
    'values':'VALUES',
    'value':'VALUE',
    'varchar':'VARCHAR',
    'view':'VIEW',
    'when':'WHEN',
    'where':'WHERE',
    'while':'WHILE',
    'with':'WITH',
    'work':'WORK',
    'xor':'XOR',
    }

  tokens = [
    'DELIMITER',
    'DELIM',
    'NULL_SAFE',
    'OR',
    'AND',
    'LOGICAL_NOT',
    'BITWISE_AND',
    'BITWISE_OR',
    'BITWISE_XOR',
    'PLUS',
    'MINUS',
    'EQ',
    'LT',
    'GT',
    'GE',
    'LE',
    'NE',
    'SHR',
    'SHL',
    'TIMES',
    'DIV',
    'MOD',
    'COMMENTS',
    'STRING',
    'FLOAT_LIT',
    'NUMBER',
    'IDENT',
    ] + list(reserved.values())

  precedence = (
    ('left', 'OR'),
    ('left', 'AND', 'XOR'),
    ('left', 'BITWISE_OR'),
    ('left', 'BITWISE_XOR'),
    ('left', 'BITWISE_AND'),
    ('left', 'NOT'),
    ('left', 'EQ', 'NE'),
    ('left', 'LT', 'GT', 'LE', 'GE'),
    ('left', 'SHL', 'SHR'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIV', 'MOD'),
  )

  literals = ',();*@'

  last_tokens = []
  level = 0

  # Completely ignored characters: space, tab and form feed
  t_ignore = ' \t\x0c'

  # Newlines
  def t_NEWLINE(self, t):
    r'\r?\n+'
    t.lexer.lineno += t.value.count("\n")

  t_DELIM = r'[;]'
  t_OR = r'\|\|'
  t_AND = r'&&'
  t_GE = r'>='
  t_LE = r'<='
  t_NE = r'<>|!='
  t_SHR = r'>>'
  t_SHL = r'<<'
  t_TIMES = r'\*'
  t_DIV = r'/'
  t_MOD = r'%'
  t_PLUS = r'\+'
  t_MINUS = r'-'
  t_EQ = r'='
  t_LT = r'<'
  t_GT = r'>'
  t_BITWISE_AND = r'&'
  t_BITWISE_OR = r'\|'
  t_BITWISE_XOR = r'^'
  t_NULL_SAFE = r'<=>'
  t_LOGICAL_NOT = r'!'

  # Comments
  def t_COMMENTS(self, t):
    r'/\*(.|[\r\n])*?\*/|--[^\n]*|\#[^\n]*'
    t.lexer.lineno += t.value.count('\n')
    return t

  # Identifiers and reserved words
  def t_FLOAT_LIT(self, t):
    r'(\d+\.\d+([Ee](\+|-)?\d+)?|\d+[Ee][+-]?\d+)'
    t.value = float(t.value)
    return t

  def t_NUMBER(self, t):
    r'-?\d+'
    t.value = int(t.value)
    return t

  def t_STRING(self, t):
    r'"(\\"|""|[^"])*"|\'(\\\'|\'\'|[^\'])*\''
    if t.value[0] == '\'':
      t.value = t.value[1:len(t.value)-1].replace('\'\'','\\\'')
    else:
      t.value = t.value[1:len(t.value)-1].replace('""','"')
    t.lexer.lineno += t.value.count("\n")
    return t

  def t_DELIMITER(self, t):
    r'(?i)DELIMITER[ \t]+(?P<new_delim>[^ \t\n]+)'
    t.value = t.lexer.lexmatch.group('new_delim')
    return t

  def t_IDENT(self, t):
    r'[A-Za-z_][\w_.]*|`(``|[^`])*`'
    if t.value[0] == '`':
      t.value = t.value[1:-1].replace('``', '`')
    elif t.value.upper() in ['AND','OR','DIV','MOD']:
      t.type = t.value.upper()
    else:
      t.type = self.reserved.get(t.value.lower(),'IDENT')
    return t

  def t_error(self, t):
    print("Illegal character %s" % repr(t.value[0]))
    t.lexer.skip(1)

  # yacc section -----------------------------------------------------------

  def p_error(self, t):
    if not t:
      raise SQLError("Syntax Error at end of file. Possibly due to missing semicolon(';'), braces('}') or parens(')')", None)
    else:
      location = Location(self.lexer, t.lineno, t.lexpos)
      raise SQLError("invalid syntax", location)

  # yacc functions ---------------------------------------------------------

  def p_delim(self, p):
    """delim : DELIM
             | ';' """
    pass

  def p_not_opt_empty(self, p):
    """not_opt : """
    p[0] = lambda x: x

  def p_not_opt(self, p):
    """not_opt : NOT
               | LOGICAL_NOT """
    p[0] = lambda x: ('not',x)

  def p_escape_opt_empty(self, p):
    """escape_opt : """
    p[0] = lambda x: x

  def p_escape_opt(self, p):
    """escape_opt : ESCAPE simple_expr """
    p[0] = lambda x: ('escape',x,p[2])

  def p_expr_or(self, p):
    """expr : expr OR expr """
    p[0] = ('or',p[1],p[3])

  def p_expr_and(self, p):
    """expr : expr AND expr """
    p[0] = ('and',p[1],p[3])

  def p_expr_xor(self, p):
    """expr : expr XOR expr """
    p[0] = ('xor',p[1],p[3])

  def p_expr_not(self, p):
    """expr : NOT expr """
    p[0] = ('not',p[2])

  def p_true_false_or_unknown1(self, p):
    """true_false_or_unknown : TRUE """
    p[0] = True

  def p_true_false_or_unknown2(self, p):
    """true_false_or_unknown : FALSE """
    p[0] = False

  def p_true_false_or_unknown3(self, p):
    """true_false_or_unknown : UNKNOWN """
    p[0] = 'UNKNOWN'

  def p_expr_boolean_primary_is(self, p):
    """expr : boolean_primary IS not_opt true_false_or_unknown """
    p[0] = ('is',p[1],p[3](p[4]))

  def p_expr_boolean_primary(self, p):
    """expr : boolean_primary """
    p[0] = p[1]

  def p_boolean_primary_is(self, p):
    """boolean_primary : boolean_primary IS not_opt NULL """
    p[0] = ('is',p[1],p[3](None))

  def p_boolean_primary_imply(self, p):
    """boolean_primary : boolean_primary NULL_SAFE predicate """
    p[0] = ('<=>',p[1],p[3])

  def p_boolean_primary_op(self, p):
    """boolean_primary : boolean_primary comparison_operator predicate """
    p[0] = (p[2],p[1],p[3])

  def p_all_or_any1(self, p):
    """all_or_any : ALL """
    p[0] = 'all'

  def p_all_or_any2(self, p):
    """all_or_any : ANY
                  | SOME """
    p[0] = 'any'

  def p_boolean_primary_all_any(self, p):
    """boolean_primary : boolean_primary comparison_operator all_or_any '(' subquery ')' """
    p[0] = (p[2],p[1],(p[3],p[5]))

  def p_boolean_primary_pred(self, p):
    """boolean_primary : predicate """
    p[0] = p[1]

  def p_comparison_operator(self, p):
    """comparison_operator : EQ
                           | GE
                           | GT
                           | LE
                           | LT
                           | NE """
    p[0] = p[1]

  def p_expr_list_single(self, p):
    """expr_list : expr """
    p[0] = [p[1]]

  def p_expr_list(self, p):
    """expr_list : expr ',' expr_list """
    p[0] = [p[1]] + p[3]

  def p_predicate_in_subquery(self, p):
    """predicate : bit_expr not_opt IN '(' subquery ')' """
    p[0] = p[2](('in',p[1],p[5]))

  def p_predicate_in(self, p):
    """predicate : bit_expr not_opt IN '(' expr_list ')' """
    p[0] = p[2](('in',p[1],p[5]))

  def p_predicate_between(self, p):
    """predicate : bit_expr not_opt BETWEEN bit_expr AND predicate """
    p[0] = p[2](('between',p[1],p[4],p[6]))

  def p_predicate_sounds_like(self, p):
    """predicate : bit_expr SOUNDS LIKE bit_expr """
    p[0] = ('sounds',p[1],p[4])

  def p_predicate_like(self, p):
    """predicate : bit_expr not_opt LIKE simple_expr escape_opt """
    p[0] = p[2](('like',p[1],p[5](p[4])))

  def p_predicate_regexp(self, p):
    """predicate : bit_expr not_opt REGEXP bit_expr """
    p[0] = p[2](('regexp',p[1],p[4]))

  def p_predicate_bit_expr(self, p):
    """predicate : bit_expr """
    p[0] = p[1]

  def p_bit_expr_or(self, p):
    """bit_expr : bit_expr BITWISE_OR bit_expr """
    p[0] = ('|',p[1],p[3])

  def p_bit_expr_and(self, p):
    """bit_expr : bit_expr BITWISE_AND bit_expr """
    p[0] = ('&',p[1],p[3])

  def p_bit_expr_shl(self, p):
    """bit_expr : bit_expr SHL bit_expr """
    p[0] = ('<<',p[1],p[3])

  def p_bit_expr_shr(self, p):
    """bit_expr : bit_expr SHR bit_expr """
    p[0] = ('>>',p[1],p[3])

  def p_bit_expr_plus(self, p):
    """bit_expr : bit_expr PLUS bit_expr """
    p[0] = ('+',p[1],p[3])

  def p_bit_expr_minus(self, p):
    """bit_expr : bit_expr MINUS bit_expr """
    p[0] = ('-',p[1],p[3])

  def p_bit_expr_mult(self, p):
    """bit_expr : bit_expr TIMES bit_expr """
    p[0] = ('*',p[1],p[3])

  def p_bit_expr_div(self, p):
    """bit_expr : bit_expr DIV bit_expr """
    p[0] = ('/',p[1],p[3])

  def p_bit_expr_mod(self, p):
    """bit_expr : bit_expr MOD bit_expr """
    p[0] = ('%',p[1],p[3])

  def p_bit_expr_inv(self, p):
    """bit_expr : bit_expr BITWISE_XOR bit_expr """
    p[0] = ('^',p[1],p[3])

  def p_bit_expr_simple_expr(self, p):
    """bit_expr : simple_expr """
    p[0] = p[1]

  def p_literal(self, p):
    """literal : STRING
               | NUMBER
               | FLOAT_LIT """
    if isinstance(p[1], basestring):
      p[0] = "'" + str(p[1]) + "'"
    else:
      p[0] = str(p[1])

  def p_simple_expr_literal(self, p):
    """simple_expr : literal """
    p[0] = p[1]

  def p_simple_expr_ident(self, p):
    """simple_expr : IDENT """
    p[0] = p[1]

  def p_simple_expr_at_ident(self, p):
    """simple_expr : '@' IDENT """
    p[0] = '@{0}'.format(p[2])

  def p_simple_expr_convert(self, p):
    """simple_expr : function_call """
    p[0] = p[1]

  def p_subquery(self, p):
    """subquery : select """
    p[0] = p[1][0]

  def p_simple_expr_subquery(self, p):
    """simple_expr : '(' subquery ')' """
    p[0] = ('(',p[2])

  def p_simple_expr_exists(self, p):
    """simple_expr : EXISTS '(' subquery ')' """
    p[0] = ('exists',p[3])

  def p_simple_expr_paren(self, p):
    """simple_expr : '(' expr_list ')' """
    p[0] = ('(',p[2])

  def p_simple_expr_row(self, p):
    """simple_expr : ROW '(' expr_list ')' """
    p[0] = ('row',p[2])

  def p_convert_function(self, p):
    """convert_function : CONVERT '(' expr USING IDENT ')' """
    p[0] = ('convert',p[3],p[5])

  def p_cast_type_char(self, p):
    """cast_type : character_statement """
    p[0] = p[1]

  def p_cast_type_date(self, p):
    """cast_type : DATE """
    p[0] = {'type':'date'}

  def p_cast_type_datetime(self, p):
    """cast_type : DATETIME """
    p[0] = {'type':'datetime'}

  def p_cast_type_decimal(self, p):
    """cast_type : DECIMAL bidimensional_limit_opt """
    p[0] = dict({'type':'decimal'},**p[2])

  def p_signed_or_unsigned(self, p):
    """signed_or_unsigned : SIGNED """
    p[0] = {}

  def p_signed_or_unsigned_unsigned(self, p):
    """signed_or_unsigned : UNSIGNED """
    p[0] = {'unsigned':True}

  def p_ignore_integer_empty(self, p):
    """ignore_integer : """
    pass

  def p_ignore_integer(self, p):
    """ignore_integer : INTEGER """
    pass

  def p_cast_type_integer(self, p):
    """cast_type : signed_or_unsigned ignore_integer """
    p[0] = dict({'type':'integer'},**p[1])

  def p_cast_type_time(self, p):
    """cast_type : TIME """
    p[0] = {'type':'time'}

  def p_cast_function(self, p):
    """cast_function : CAST '(' expr AS cast_type ')' """
    p[0] = ('cast',p[3],p[5])

  def p_opt_distinct_empty(self, p):
    """opt_distinct : """
    p[0] = lambda x: x

  def p_opt_distinct(self, p):
    """opt_distinct : DISTINCT """
    p[0] = lambda x: ('distinct',x)

  def p_average_function(self, p):
    """average_function : AVG '(' opt_distinct expr ')' """
    p[0] = ('avg',p[3](p[4]))

  def p_bit_function(self, p):
    """bit_function : BIT_AND '(' expr ')'
                    | BIT_OR '(' expr ')'
                    | BIT_XOR '(' expr ')' """
    p[0] = (p[1].lower(),p[3])

  def p_opt_all(self, p):
    """opt_all : ALL
               | """
    pass

  def p_count_function_wild(self, p):
    """count_function : COUNT '(' opt_all '*' ')' """
    p[0] = ('count','*')

  def p_count_function(self, p):
    """count_function : COUNT '(' expr ')' """
    p[0] = ('count',p[3])

  def p_count_function_distinct(self, p):
    """count_function : COUNT '(' DISTINCT expr_list ')' """
    p[0] = ('count',('distinct',p[4]))

  def p_max_function(self, p):
    """max_function : MAX '(' opt_distinct expr ')' """
    p[0] = ('max',p[3](p[4]))

  def p_min_function(self, p):
    """min_function : MIN '(' opt_distinct expr ')' """
    p[0] = ('min',p[3](p[4]))

  def p_sum_function(self, p):
    """sum_function : SUM '(' opt_distinct expr ')' """
    p[0] = ('sum',p[3](p[4]))

  def p_other_function_empty(self, p):
    """other_function : IDENT '(' ')' """
    p[0] = (p[1].lower())

  def p_other_function(self, p):
    """other_function : IDENT '(' expr_list ')' """
    p[0] = tuple([p[1].lower()] + p[3])

  def p_function_call(self, p):
    """function_call : convert_function
                     | cast_function
                     | average_function
                     | bit_function
                     | count_function
                     | max_function
                     | min_function
                     | sum_function
                     | other_function """
    p[0] = p[1]

  def p_ignore_comma(self, p):
    """ignore_comma : ','
                    | """
    pass

  def p_ignore_eq(self, p):
    """ignore_eq : EQ
                 | """
    pass

  def p_ignore_index_or_key(self, p):
    """ignore_index_or_key : KEY
                           | INDEX
                           | """
    pass

  def p_ignore_key(self, p):
    """ignore_key : KEY
                  | """
    pass

  def p_ignore_default(self, p):
    """ignore_default : DEFAULT
                      | """
    pass

  def p_ignore_column(self, p):
    """ignore_column : COLUMN
                     | """
    pass

  def p_ignore_to_or_as(self, p):
    """ignore_to_or_as : TO
                       | AS
                       | """
    pass

  def p_check_index_or_key(self, p):
    """check_index_or_key : INDEX
                          | KEY """
    pass

  def p_constraint_empty(self, p):
    """constraint_opt : """
    p[0] = {}

  def p_constraint(self, p):
    """constraint_opt : CONSTRAINT IDENT """
    p[0] = {'name':p[2]}

  def p_column_name(self, p):
    """column_name : IDENT """
    p[0] = p[1]

  def p_column_names_single(self, p):
    """column_names : column_name """
    p[0] = [p[1]]

  def p_column_names(self, p):
    """column_names : column_name ',' column_names """
    p[0] = [p[1]] + p[3]

  def p_primary_key_statement(self, p):
    """primary_key_statement : constraint_opt PRIMARY KEY '(' column_names ')' """
    if len(p[1]) == 0:
      p[0] = ('primary_key',p[5])
    else:
      p[0] = ('primary_key',p[5],p[1])

  def p_index_statement(self, p):
    """index_statement : INDEX IDENT '(' column_names ')'
                       | KEY IDENT '(' column_names ')' """
    p[0] = ('index',p[4],{'name':p[1]})

  def p_index_name_opt_empty(self, p):
    """index_name_opt : """
    p[0] = {}

  def p_index_name_opt(self, p):
    """index_name_opt : IDENT """
    p[0] = {'name':p[1]}

  def p_unique_statement(self, p):
    """unique_statement : constraint_opt UNIQUE ignore_index_or_key index_name_opt '(' column_names ')' """
    args = dict(p[1],**p[4])
    if len(args) == 0:
      p[0] = ('unique',p[6])
    else:
      p[0] = ('unique',p[6],args)

  def p_col_name(self, p):
    """col_name : IDENT """
    p[0] = p[1]

  def p_integer_type_tinyint(self, p):
    """integer_type : TINYINT """
    p[0] = 'tinyinteger'

  def p_integer_type_smallint(self, p):
    """integer_type : SMALLINT """
    p[0] = 'smallinteger'

  def p_integer_type_mediumint(self, p):
    """integer_type : MEDIUMINT """
    p[0] = 'mediuminteger'

  def p_integer_type_int(self, p):
    """integer_type : INT
                    | INTEGER """
    p[0] = 'integer'

  def p_integer_type_bigint(self, p):
    """integer_type : BIGINT """
    p[0] = 'biginteger'

  def p_double_precision_float(self, p):
    """double_precision_type : FLOAT """
    p[0] = 'float'

  def p_double_precision_decimal(self, p):
    """double_precision_type : DECIMAL """
    p[0] = 'decimal'

  def p_double_precision_numeric(self, p):
    """double_precision_type : NUMERIC """
    p[0] = 'numeric'

  def p_limit_opt_empty(self, p):
    """limit_opt : """
    p[0] = {}

  def p_limit_opt(self, p):
    """limit_opt : limit """
    p[0] = p[1]

  def p_limit(self, p):
    """limit : '(' NUMBER ')' """
    p[0] = {'limit':p[2]}

  def p_bidimensional_limit_opt_empty(self, p):
    """bidimensional_limit_opt : """
    p[0] = {}

  def p_bidimensional_limit_opt(self, p):
    """bidimensional_limit_opt : bidimensional_limit """
    p[0] = p[1]

  def p_bidimensional_limit(self, p):
    """bidimensional_limit : '(' NUMBER ',' NUMBER ')' """
    p[0] = {'precision':p[2],'scale':p[4]}

  def p_charset_statement(self, p):
    """charset_statement : CHARACTER SET IDENT """
    p[0] = {}

  def p_collate_statement(self, p):
    """collate_statement : COLLATE IDENT """
    p[0] = {'collate':p[2]}

  def p_null_null(self, p):
    """null : NULL """
    p[0] = {'null':True}

  def p_null_not_null(self, p):
    """null : NOT NULL """
    p[0] = {'null':False}

  def p_key_unique(self, p):
    """key : UNIQUE ignore_key """
    p[0] = {'unique':True}

  def p_key_primary_key(self, p):
    """key : KEY
           | PRIMARY KEY """
    p[0] = {'primary_key':True}

  def p_auto_increment(self, p):
    """auto_increment : AUTO_INCREMENT """
    p[0] = {'auto_increment':True}

  def p_default(self, p):
    """default : DEFAULT value """
    p[0] = {'default':p[2]}

  def p_value_null(self, p):
    """value : NULL """
    p[0] = None

  def p_value(self, p):
    """value : STRING
             | NUMBER
             | FLOAT_LIT
             | IDENT """
    p[0] = p[1]

  def p_unsigned_opt_empty(self, p):
    """unsigned_opt : """
    p[0] = {}

  def p_unsigned_opt(self, p):
    """unsigned_opt : UNSIGNED """
    p[0] = {'unsigned':True}

  def p_integer_statement(self, p):
    """integer_statement : integer_type limit_opt unsigned_opt """
    result = dict({'type':p[1]},**p[2])
    result.update(p[3])
    if p[1] == 'integer' and 'limit' in p[2] and p[2]['limit'] == 11:
      del result['limit']
    if p[1] == 'tinyinteger' and 'limit' in p[2] and p[2]['limit'] == 1:
      del result['limit']
      result['type'] = 'boolean'
    p[0] = result

  def p_double_precision_statement(self, p):
    """double_precision_statement : double_precision_type bidimensional_limit_opt unsigned_opt """
    result = dict({'type':p[1]},**p[2])
    result.update(p[3])
    p[0] = result

  def p_check_char_or_character(self, p):
    """check_char_or_character : CHAR
                               | CHARACTER """
    pass

  def p_character_statement(self, p):
    """character_statement : check_char_or_character limit_opt """
    p[0] = dict({'type':'character'},**p[2])

  def p_string_statement(self, p):
    """string_statement : VARCHAR limit """
    p[0] = dict({'type':'string'},**p[2])
    if p[0]['limit'] == 255:
      del p[0]['limit']

  def p_text_type(self, p):
    """text_type : TINYTEXT
                 | TEXT
                 | MEDIUMTEXT
                 | LONGTEXT """
    p[0] = p[1].lower()

  def p_text_statement(self, p):
    """text_statement : text_type """
    p[0] = {'type':p[1]}

  def p_simple_statement(self, p):
    """simple_statement : DATE
                         | TIME
                         | TIMESTAMP
                         | DATETIME """
    p[0] = {'type':p[1].lower()}

  def p_binary_statement_tinyblob(self, p):
    """binary_statement : TINYBLOB """
    p[0] = {'type':'tinybinary'}

  def p_binary_statement_blob(self, p):
    """binary_statement : BLOB """
    p[0] = {'type':'binary'}

  def p_binary_statement_mediumblob(self, p):
    """binary_statement : MEDIUMBLOB """
    p[0] = {'type':'mediumbinary'}

  def p_binary_statement_longblob(self, p):
    """binary_statement : LONGBLOB """
    p[0] = {'type':'longbinary'}

  def p_data_type(self, p):
    """data_type : integer_statement
                 | double_precision_statement
                 | character_statement
                 | string_statement
                 | text_statement
                 | simple_statement
                 | binary_statement """
    p[0] = p[1]

  def p_column_option(self, p):
    """column_option : null
                     | default
                     | auto_increment
                     | key
                     | charset_statement
                     | collate_statement """
    p[0] = p[1]

  def p_column_options_empty(self, p):
    """column_options : """
    p[0] = {}

  def p_column_options(self, p):
    """column_options : column_option column_options """
    p[0] = dict(p[1],**p[2])

  def p_column_statement(self, p):
    """column_statement : data_type column_options """
    p[0] = dict(p[1],**p[2])

  def p_create_statement_column(self, p):
    """create_statement : col_name column_statement """
    t = p[2].pop('type')
    if len(p[2]) > 0:
      p[0] = ('column',p[1],t,p[2])
    else:
      p[0] = ('column',p[1],t)

  def p_create_statement(self, p):
    """create_statement : primary_key_statement
                        | index_statement
                        | unique_statement """
    p[0] = p[1]

  def p_create_statements_single(self, p):
    """create_statements : create_statement """
    p[0] = [p[1]]

  def p_create_statements(self, p):
    """create_statements : create_statement ',' create_statements """
    p[0] = [p[1]] + p[3]

  def p_table_option_engine(self, p):
    """table_option : ENGINE ignore_eq IDENT"""
    p[0] = ('engine',p[3])

  def p_table_option_auto_increment(self, p):
    """table_option : AUTO_INCREMENT ignore_eq NUMBER """
    p[0] = ('auto_increment',p[3])

  def p_table_option_charset_1(self, p):
    """table_option : ignore_default CHARACTER SET ignore_eq IDENT """
    p[0] = ('charset',p[5])

  def p_table_option_charset_2(self, p):
    """table_option : ignore_default CHARSET ignore_eq IDENT """
    p[0] = ('charset',p[4])

  def p_table_option_collate(self, p):
    """table_option : ignore_default COLLATE ignore_eq IDENT """
    p[0] = ('collate',p[4])

  def p_table_options_opt_empty(self, p):
    """table_options_opt : """
    p[0] = []

  def p_table_options_opt(self, p):
    """table_options_opt : table_options """
    p[0] = p[1]

  def p_table_options_single(self, p):
    """table_options : table_option """
    p[0] = [p[1]]

  def p_table_options(self, p):
    """table_options : table_option ignore_comma table_options """
    p[0] = [p[1]] + p[3]

  def p_if_not_exists_empty(self, p):
    """if_not_exists : """
    p[0] = {}

  def p_if_not_exists(self, p):
    """if_not_exists : IF NOT EXISTS """
    p[0] = {'if_not_exists':True}

  def p_create_table(self, p):
    """create_table : CREATE TABLE if_not_exists IDENT '(' create_statements ')' table_options_opt """
    # optimize single primary keys
    columns = []
    d = {}
    for statement in p[6]:
      if 'column' == statement[0]:
        d[statement[1]] = statement # index column
        columns.append(statement)
      elif 'primary_key' == statement[0] and len(statement[1]) == 1:
        d[statement[1][0]][3]['primary_key'] = True
      else:
        columns.append(statement)
    args = dict({'location':self._location(p)},**p[3])
    if len(p[8]) > 0:
      args.update({'options':p[8]})
    if len(args) > 0:
      p[0] = [('create_table',p[4],columns,args)]
    else:
      p[0] = [('create_table',p[4],columns)]

  def p_temporary_empty(self, p):
    """temporary : """
    p[0] = {}

  def p_temporary(self, p):
    """temporary : TEMPORARY """
    p[0] = {'temporary':True}

  def p_if_exists_empty(self, p):
    """if_exists : """
    p[0] = {}

  def p_if_exists(self, p):
    """if_exists : IF EXISTS """
    p[0] = {'if_exists':True}

  def p_opt_restrict_cascade_empty(self, p):
    """opt_restrict_cascade : """
    p[0] = {}

  def p_opt_restrict_cascade1(self, p):
    """opt_restrict_cascade : RESTRICT """
    p[0] = {'restrict':True}

  def p_opt_restrict_cascade2(self, p):
    """opt_restrict_cascade : CASCADE """
    p[0] = {'cascade':True}

  def p_drop_table(self, p):
    """drop_table : DROP temporary TABLE if_exists ident_list opt_restrict_cascade """
    args = dict({'location':self._location(p)},**p[2])
    args.update(p[4])
    args.update(p[6])
    p[0] = [('drop_table',p[5],args)]

  def p_position_opt_empty(self, p):
    """position_opt : """
    p[0] = {}

  def p_position_opt(self, p):
    """position_opt : position """
    p[0] = p[1]

  def p_position_first(self, p):
    """position : FIRST """
    p[0] = {'first':True}

  def p_position_after(self, p):
    """position : AFTER IDENT """
    p[0] = {'after':p[2]}

  def p_add_column(self, p):
    """add_column : ADD ignore_column column_name column_statement position_opt """
    args = dict({'location':self._location(p)},**p[4])
    args.update(p[5])
    p[0] = ('add_column',None,p[3],args.pop('type'),args)

  def p_add_index(self, p):
    """add_index : ADD check_index_or_key index_name_opt '(' column_names ')' """
    p[0] = ('add_index',None,p[5],dict({'location':self._location(p)},**p[3]))

  def p_add_index_unique(self, p):
    """add_index : ADD constraint_opt UNIQUE ignore_index_or_key index_name_opt '(' column_names ')' """
    args = dict({'unique':True,'location':self._location(p)},**p[2])
    args.update(p[5])
    p[0] = ('add_index',None,p[7],args)

  def p_add_primary_key(self, p):
    """add_primary_key : ADD constraint_opt PRIMARY KEY '(' column_names ')' """
    p[0] = ('add_primary_key',None,p[6],dict({'location':self._location(p)},**p[2]))

  def p_change_column(self, p):
    """change_column : CHANGE ignore_column column_name column_name column_statement position_opt """
    args = dict({'location':self._location(p)},**p[5])
    args.update(p[6])
    if p[3] != p[4]:
      args['name'] = p[4]
    p[0] = ('change_column',None,p[3],args.pop('type'),args)

  def p_modify_column(self, p):
    """change_column : MODIFY ignore_column column_name column_statement position_opt """
    args = dict({'location':self._location(p)},**p[4])
    args.update(p[5])
    p[0] = ('change_column',None,p[3],args.pop('type'),args)

  def p_change_default_set(self, p):
    """change_default : ALTER ignore_column IDENT SET DEFAULT literal """
    p[0] = ('set_column_default',None,p[3],p[6],{'location':self._location(p)})

  def p_change_default_drop(self, p):
    """change_default : ALTER ignore_column IDENT DROP DEFAULT """
    p[0] = ('drop_column_default',None,p[3],{'location':self._location(p)})

  def p_remove_column(self, p):
    """remove_column : DROP ignore_column IDENT """
    p[0] = ('remove_column',None,p[3],{'location':self._location(p)})

  def p_remove_primary_key(self, p):
    """remove_primary_key : DROP PRIMARY KEY """
    p[0] = ('remove_primary_key',None,{'location':self._location(p)})

  def p_remove_index(self, p):
    """remove_index : DROP check_index_or_key IDENT """
    p[0] = ('remove_index',None,p[3],{'location':self._location(p)})

  def p_alter_rename_table(self, p):
    """alter_rename_table : RENAME ignore_to_or_as IDENT """
    p[0] = ('rename_table',None,p[3],{'location':self._location(p)})

  def p_alter_specification(self, p):
    """alter_specification : add_column
                           | add_index
                           | add_primary_key
                           | change_column
                           | change_default
                           | remove_column
                           | remove_primary_key
                           | remove_index
                           | alter_rename_table """
    p[0] = p[1]

  def p_alter_specifications_single(self, p):
    """alter_specifications : alter_specification """
    p[0] = [p[1]]

  def p_alter_specifications(self, p):
    """alter_specifications : alter_specification ',' alter_specifications """
    p[0] = [p[1]] + p[3]

  def p_alter_table(self, p):
    """alter_table : ALTER TABLE IDENT alter_specifications """
    p[0] = [tuple([x[0],p[3]] + list(x)[2:]) for x in p[4]]

  def p_ignore_into(self, p):
    """ignore_into : INTO
                   | """
    pass

  def p_check_values_or_value(self, p):
    """check_values_or_value : VALUES
                             | VALUE """
    pass

  def p_default_or_value_default(self, p):
    """default_or_value : DEFAULT """
    p[0] = 'DEFAULT'

  def p_default_or_value_value(self, p):
    """default_or_value : value """
    p[0] = p[1]

  def p_default_or_values_single(self, p):
    """default_or_values : default_or_value """
    p[0] = [p[1]]

  def p_default_or_values(self, p):
    """default_or_values : default_or_value ',' default_or_values """
    p[0] = [p[1]] + p[3]

  def p_default_or_values_list_single(self, p):
    """default_or_values_list : '(' default_or_values ')' """
    p[0] = [tuple(p[2])]

  def p_default_or_values_list(self, p):
    """default_or_values_list : '(' default_or_values ')' ',' default_or_values_list """
    p[0] = [tuple(p[2])] + p[5]

  def p_insert(self, p):
    """insert : INSERT ignore_into IDENT '(' column_names ')' check_values_or_value default_or_values_list """
    p[0] = [('insert',p[3],p[5],p[8],{'location':self._location(p)})]

  def p_insert_natural(self, p):
    """insert : INSERT ignore_into IDENT check_values_or_value default_or_values_list """
    p[0] = [('insert',p[3],[],p[5],{'location':self._location(p)})]

  def p_expr_or_default(self, p):
    """expr_or_default : expr
                       | DEFAULT """
    p[0] = p[1]

  def p_key_value_value(self, p):
    """key_value : IDENT EQ expr_or_default """
    p[0] = (p[1],p[3])

  def p_key_values_single(self, p):
    """key_values : key_value """
    p[0] = [p[1]]

  def p_key_values(self, p):
    """key_values : key_value ',' key_values """
    p[0] = [p[1]] + p[3]

  def p_where_empty(self, p):
    """where : """
    p[0] = {}

  def p_where(self, p):
    """where : WHERE expr """
    p[0] = {'where':p[2]}

  def p_ident_list_single(self, p):
    """ident_list : IDENT """
    p[0] = [p[1]]

  def p_ident_list(self, p):
    """ident_list : IDENT ',' ident_list """
    p[0] = [p[1]] + p[3]

  def p_rows_limit_empty(self, p):
    """rows_limit : """
    p[0] = {}

  def p_rows_limit(self, p):
    """rows_limit : LIMIT NUMBER """
    p[0] = {'limit':p[2]}

  def p_update(self, p):
    """update : UPDATE ident_list SET key_values where rows_limit """
    args = dict({'location':self._location(p)},**p[5])
    p[0] = [('update',p[2],p[4],args)]

  def p_delete_options_empty(self, p):
    """delete_options : """
    p[0] = {}

  def p_delete_options(self, p):
    """delete_options : delete_option delete_options """
    p[0] = dict(p[2],**p[1])

  def p_delete_option(self, p):
    """delete_option : LOW_PRIORITY
                     | QUICK
                     | IGNORE """
    p[0] = {p[1].lower():True}

  def p_delete_single(self, p):
    """delete : DELETE delete_options FROM IDENT where rows_limit """
    args = dict({'location':self._location(p)},**p[2])
    args.update(p[5])
    args.update(p[6])
    p[0] = [('delete',p[4],args)]

  def p_delete_multi(self, p):
    """delete : DELETE delete_options ident_list FROM where """
    args = dict({'location':self._location(p)},**p[2])
    args.update(p[5])
    p[0] = [('delete',p[3],args)]

  def p_ident_to_ident(self, p):
    """ident_to_ident : IDENT TO IDENT """
    p[0] = ('rename_table',p[1],p[3],{'location':self._location(p)})

  def p_rename_tables_single(self, p):
    """rename_tables : ident_to_ident """
    p[0] = [p[1]]

  def p_rename_tables(self, p):
    """rename_tables : ident_to_ident ',' rename_tables """
    p[0] = [p[1]] + p[3]

  def p_rename_table(self, p):
    """rename_table : RENAME TABLE rename_tables """
    p[0] = p[3]

  def p_sp_proc_stmt_statement(self, p):
    """sp_proc_stmt_statement : statement """
    p[0] = p[1]

  def p_sp_proc_stmt_return(self, p):
    """sp_proc_stmt_return : RETURN expr """
    p[0] = [('return',p[2])]

  def p_sp_proc_stmts1_single(self, p):
    """sp_proc_stmts1 : sp_proc_stmt delim """
    p[0] = [p[1]]

  def p_sp_proc_stmts1(self, p):
    """sp_proc_stmts1 : sp_proc_stmt delim sp_proc_stmts1 """
    p[0] = [p[1]] + p[3]

  def p_sp_if(self, p):
    """sp_if : expr THEN sp_proc_stmts1 sp_elseifs """
    p[0] = [('if',p[1],p[3],p[4])]

  def p_sp_elseifs_empty(self, p):
    """sp_elseifs : """
    p[0] = None

  def p_sp_elseifs_elseif(self, p):
    """sp_elseifs : ELSEIF sp_if """
    p[0] = p[2]

  def p_sp_elseifs_else(self, p):
    """sp_elseifs : ELSE sp_proc_stmts1 """
    p[0] = p[2]

  def p_sp_proc_stmt_if(self, p):
    """sp_proc_stmt_if : IF sp_if END IF """
    p[0] = p[2]

  def p_else_clause_opt_empty(self, p):
    """else_clause_opt : """
    p[0] = None

  def p_else_clause_opt(self, p):
    """else_clause_opt : ELSE sp_proc_stmts1 """
    p[0] = p[2]

  def p_simple_when_clause(self, p):
    """simple_when_clause : WHEN expr THEN sp_proc_stmts1 """
    p[0] = [('when',p[2],p[4])]

  def p_simple_when_clause_list_single(self, p):
    """simple_when_clause_list : simple_when_clause """
    p[0] = [p[1]]

  def p_simple_when_clause_list(self, p):
    """simple_when_clause_list : simple_when_clause simple_when_clause_list """
    p[0] = [p[1]] + p[2]

  def p_simple_case_stmt(self, p):
    """simple_case_stmt : CASE expr simple_when_clause_list else_clause_opt END CASE """
    p[0] = [('case',p[2],p[3],p[4])]

  def p_searched_when_clause(self, p):
    """searched_when_clause : WHEN expr THEN sp_proc_stmts1 """
    p[0] = [('when',p[2],p[4])]

  def p_searched_when_clause_list_single(self, p):
    """searched_when_clause_list : searched_when_clause """
    p[0] = [p[1]]

  def p_searched_when_clause_list(self, p):
    """searched_when_clause_list : searched_when_clause searched_when_clause_list """
    p[0] = [p[1]] + p[2]

  def p_searched_case_stmt(self, p):
    """searched_case_stmt : CASE searched_when_clause_list else_clause_opt END CASE """
    p[0] = [('case',p[2],p[3])]

  def p_case_stmt_specification(self, p):
    """case_stmt_specification : simple_case_stmt
                               | searched_case_stmt """
    p[0] = p[1]

  def p_sp_decls_empty(self, p):
    """sp_decls : """
    p[0] = []

  def p_sp_decls(self, p):
    """sp_decls : sp_decl delim sp_decls """
    p[0] = [p[1]] + p[3]

  def p_sp_opt_default_empty(self, p):
    """sp_opt_default : """
    p[0] = None

  def p_sp_opt_default(self, p):
    """sp_opt_default : DEFAULT expr """
    p[0] = p[2]

  def p_opt_collate_empty(self, p):
    """opt_collate : """
    p[0] = {}

  def p_sp_collation_name_or_default(self, p):
    """sp_collation_name_or_default : IDENT
                                    | DEFAULT """
    p[0] = p[1]

  def p_opt_collate(self, p):
    """opt_collate : COLLATE sp_collation_name_or_default """
    p[0] = {'collate':p[2]}

  def p_sp_type_integer(self, p):
    """sp_type : integer_type limit_opt unsigned_opt """
    result = dict({'type':p[1]},**p[2])
    result.update(p[3])
    p[0] = result

  def p_sp_type_with_opt_collate(self, p):
    """sp_type_with_opt_collate : sp_type opt_collate """
    p[0] = dict(p[1],**p[2])

  def p_sp_decl_ids(self, p):
    """sp_decl : DECLARE sp_ident_list sp_type_with_opt_collate sp_opt_default  """
    p[0] = ('declare',p[2],p[3],p[4])

  def p_sp_proc_stmts_empty(self, p):
    """sp_proc_stmts : """
    p[0] = []

  def p_sp_proc_stmts(self, p):
    """sp_proc_stmts : sp_proc_stmt delim sp_proc_stmts """
    p[0] = p[1] + p[3]

  def p_sp_block_content(self, p):
    """sp_block_content : BEGIN sp_decls sp_proc_stmts END """
    p[0] = [('begin',p[2],p[3])]

  def p_sp_opt_label_empty(self, p):
    """sp_opt_label : """
    pass

  def p_sp_opt_label(self, p):
    """sp_opt_label : IDENT """
    pass

  def p_sp_labeled_block(self, p):
    """sp_labeled_block : IDENT ':' sp_block_content sp_opt_label """
    p[0] = [('label',p[1],p[3])]

  def p_sp_unlabeled_block(self, p):
    """sp_unlabeled_block : sp_block_content """
    p[0] = p[1]

  def p_sp_unlabeled_control_loop(self, p):
    """sp_unlabeled_control : LOOP sp_proc_stmts1 END LOOP """
    p[0] = [('loop',p[2])]

  def p_sp_unlabeled_control_while(self, p):
    """sp_unlabeled_control : WHILE expr DO sp_proc_stmts1 END WHILE """
    p[0] = [('while',p[2],p[4])]

  def p_sp_unlabeled_control_repeat(self, p):
    """sp_unlabeled_control : REPEAT sp_proc_stmts1 UNTIL expr END REPEAT """
    p[0] = [('repeat',p[2],p[4])]

  def p_sp_labeled_control(self, p):
    """sp_labeled_control : IDENT ':' sp_unlabeled_control sp_opt_label """
    p[0] = [('label',p[1],p[3])]

  def p_sp_proc_stmt_unlabeled(self, p):
    """sp_proc_stmt_unlabeled : sp_unlabeled_control """
    p[0] = p[1]

  def p_sp_proc_stmt_leave(self, p):
    """sp_proc_stmt_leave : LEAVE IDENT """
    p[0] = [('leave',p[2])]

  def p_sp_proc_stmt_iterate(self, p):
    """sp_proc_stmt_iterate : ITERATE IDENT """
    p[0] = [('iterate',p[2])]

  def p_sp_proc_stmt_open(self, p):
    """sp_proc_stmt_open : OPEN IDENT """
    p[0] = [('open',p[2])]

  def p_ignore_fetch_noise(self, p):
    """ignore_fetch_noise : NEXT FROM
                          | FROM
                          | """
    pass

  def p_sp_ident_list_single(self, p):
    """sp_ident_list : IDENT """
    p[0] = [p[1]]

  def p_fetch_list(self, p):
    """sp_ident_list : IDENT ',' sp_ident_list """
    p[0] = [p[1]] + p[3]

  def p_sp_proc_stmt_fetch(self, p):
    """sp_proc_stmt_fetch : FETCH ignore_fetch_noise IDENT INTO sp_ident_list """
    p[0] = [('fetch',p[3],p[5])]

  def p_sp_proc_stmt_close(self, p):
    """sp_proc_stmt_close : CLOSE IDENT """
    p[0] = [('close',p[2])]

  def p_sp_proc_stmt(self, p):
    """sp_proc_stmt : sp_proc_stmt_statement
                    | sp_proc_stmt_return
                    | sp_proc_stmt_if
                    | case_stmt_specification
                    | sp_labeled_block
                    | sp_unlabeled_block
                    | sp_labeled_control
                    | sp_proc_stmt_unlabeled
                    | sp_proc_stmt_leave
                    | sp_proc_stmt_iterate
                    | sp_proc_stmt_open
                    | sp_proc_stmt_fetch
                    | sp_proc_stmt_close """
    p[0] = p[1]

  def p_trigger_event(self, p):
    """trigger_event : INSERT
                     | UPDATE
                     | DELETE """
    p[0] = p[1].lower()

  def p_trigger_time(self, p):
    """trigger_time : BEFORE
                    | AFTER """
    p[0] = p[1].lower()

  def p_create_trigger(self, p):
    """create_trigger : CREATE TRIGGER IDENT trigger_time trigger_event ON IDENT FOR EACH ROW sp_proc_stmt """
    p[0] = [('create_trigger',p[7],p[3],p[4],p[5],p[11],{'location':self._location(p)})]

  def p_select_options_empty(self, p):
    """select_options : """
    p[0] = {}

  def p_select_options(self, p):
    """select_options : select_option_list """
    p[0] = p[1]

  def p_select_option_distinct(self, p):
    """select_option : DISTINCT """
    p[0] = {'distinct':True}

  def p_select_option_all(self, p):
    """select_option : ALL """
    p[0] = {'all':True}

  def p_select_option_list_single(self, p):
    """select_option_list : select_option """
    p[0] = p[1]

  def p_select_option_list(self, p):
    """select_option_list : select_option select_option_list """
    p[0] = dict(p[2],**p[1])

  def p_select_alias_empty(self, p):
    """select_alias : """
    p[0] = {}

  def p_select_alias_as(self, p):
    """select_alias : AS IDENT """
    p[0] = {'alias':p[2]}

  def p_select_alias_no_as(self, p):
    """select_alias : IDENT """
    p[0] = {'alias':p[1]}

  def p_table_wild(self, p):
    """table_wild : IDENT '*' """
    p[0] = '{0}*'.format(p[1])

  def p_select_item2(self, p):
    """select_item2 : table_wild
                    | expr """
    p[0] = p[1]

  def p_select_item(self, p):
    """select_item : select_item2 select_alias """
    if len(p[2]) == 0:
      p[0] = ('item',p[1])
    else:
      p[0] = ('item',p[1],p[2])

  def p_select_item_list_single(self, p):
    """select_item_list : select_item """
    p[0] = [p[1]]

  def p_select_item_list(self, p):
    """select_item_list : select_item ',' select_item_list """
    p[0] = [p[1]] + p[3]

  def p_select_item_list_star(self, p):
    """select_item_list : '*' """
    p[0] = ['*']

  def p_opt_order_clause_empty(self, p):
    """opt_order_clause : """
    p[0] = {}

  def p_opt_order_clause(self, p):
    """opt_order_clause : order_clause """
    p[0] = p[1]

  def p_order_clause(self, p):
    """order_clause : ORDER BY order_list """
    p[0] = {'order_by':p[3]}

  def p_order_ident(self, p):
    """order_ident : expr """
    p[0] = p[1]

  def p_order_dir_empty(self, p):
    """order_dir : """
    p[0] = None

  def p_order_dir_asc(self, p):
    """order_dir : ASC """
    p[0] = 'asc'

  def p_order_dir_desc(self, p):
    """order_dir : DESC """
    p[0] = 'desc'

  def p_order_item(self, p):
    """order_item : order_ident order_dir """
    if p[2] == None:
      p[0] = p[1]
    else:
      p[0] = (p[1],p[2])

  def p_order_list_single(self, p):
    """order_list : order_item """
    p[0] = [p[1]]

  def p_order_list(self, p):
    """order_list : order_item ',' order_list """
    p[0] = [p[1]] + p[3]

  def p_opt_limit_clause_empty(self, p):
    """opt_limit_clause : """
    p[0] = {}

  def p_opt_limit_clause(self, p):
    """opt_limit_clause : limit_clause """
    p[0] = p[1]

  def p_limit_clause(self, p):
    """limit_clause : LIMIT limit_options """
    p[0] = p[2]

  def p_limit_options_single(self, p):
    """limit_options : limit_option """
    p[0] = {'limit':p[1]}

  def p_limit_options_two(self, p):
    """limit_options : limit_option ',' limit_option """
    p[0] = {'offset':p[1],'limit':p[3]}

  def p_limit_options_offset(self, p):
    """limit_options : limit_option OFFSET limit_option """
    p[0] = {'offset':p[3],'limit':p[1]}

  def p_limit_option_ident(self, p):
    """limit_option : IDENT """
    p[0] = p[1]

  def p_limit_option_number(self, p):
    """limit_option : NUMBER """
    p[0] = p[1]

  def p_select_into_order_limit(self, p):
    """select_into : opt_order_clause opt_limit_clause """
    p[0] = dict(p[1],**p[2])

  def p_select_var_list_single(self, p):
    """select_var_list : select_var_ident """
    p[0] = [p[1]]

  def p_select_var_list(self, p):
    """select_var_list : select_var_ident ',' select_var_list """
    p[0] = [p[1]] + p[3]

  def p_select_var_ident1(self, p):
    """select_var_ident : '@' IDENT """
    p[0] = '@{0}'.format(p[2])

  def p_select_var_ident2(self, p):
    """select_var_ident : IDENT """
    p[0] = p[1]

  def p_into_destination(self, p):
    """into_destination : select_var_list """
    p[0] = p[1]

  def p_into(self, p):
    """into : INTO into_destination """
    p[0] = {'into':p[2]}

  def p_select_into(self, p):
    """select_into : into """
    p[0] = p[1]

  def p_table_alias_empty(self, p):
    """table_alias : """
    pass

  def p_table_alias_as(self, p):
    """table_alias : AS """
    pass

  def p_table_alias_eq(self, p):
    """table_alias : EQ """
    pass

  def p_opt_table_alias_empty(self, p):
    """opt_table_alias : """
    p[0] = {}

  def p_opt_table_alias(self, p):
    """opt_table_alias : table_alias IDENT """
    p[0] = {'alias':p[2]}

  def p_index_hint_clause_empty(self, p):
    """index_hint_clause : """
    p[0] = {}

  def p_index_hint_clause_for_join(self, p):
    """index_hint_clause : FOR JOIN """
    p[0] = {'index_hint_clause':'for_join'}

  def p_index_hint_clause_for_order_by(self, p):
    """index_hint_clause : FOR ORDER BY """
    p[0] = {'index_hint_clause':'for_order_by'}

  def p_index_hint_clause_for_group_by(self, p):
    """index_hint_clause : FOR GROUP BY """
    p[0] = {'index_hint_clause':'for_group_by'}

  def p_index_hint_type_force(self, p):
    """index_hint_type : FORCE """
    p[0] = {'index_hint_type':'force'}

  def p_index_hint_type_ignore(self, p):
    """index_hint_type : IGNORE """
    p[0] = {'index_hint_type':'ignore'}

  def p_index_hint_type_use(self, p):
    """index_hint_type : USE """
    p[0] = {'index_hint_type':'use'}

  def p_opt_key_usage_list_empty(self, p):
    """opt_key_usage_list : """
    p[0] = []

  def p_opt_key_usage_list(self, p):
    """opt_key_usage_list : key_usage_list """
    p[0] = p[1]

  def p_key_usage_element_ident(self, p):
    """key_usage_element : IDENT """
    p[0] = p[1]

  def p_key_usage_element_primary(self, p):
    """key_usage_element : PRIMARY """
    p[0] = p[1]

  def p_key_usage_list_single(self, p):
    """key_usage_list : key_usage_element """
    p[0] = [p[1]]

  def p_key_usage_list(self, p):
    """key_usage_list : key_usage_element ',' key_usage_list """
    p[0] = [p[1]] + p[3]

  def p_index_hint_definition(self, p):
    """index_hint_definition : index_hint_type check_index_or_key index_hint_clause '(' opt_key_usage_list ')' """
    p[0] = dict(p[1],**p[3])

  def p_index_hints_list_single(self, p):
    """index_hints_list : index_hint_definition """
    p[0] = [p[1]]

  def p_index_hints_list(self, p):
    """index_hints_list : index_hint_definition index_hints_list """
    p[0] = [p[1]] + p[2]

  def p_opt_index_hints_list_empty(self, p):
    """opt_index_hints_list : """
    p[0] = []

  def p_opt_index_hints_list(self, p):
    """opt_index_hints_list : index_hints_list """
    p[0] = p[1]

  def p_opt_key_definition(self, p):
    """opt_key_definition : opt_index_hints_list """
    p[0] = p[1]

  def p_table_factor(self, p):
    """table_factor : IDENT opt_table_alias opt_key_definition """
    args = p[2]
    if len(p[3]) > 0:
      args['keys'] = p[3]
    if len(args) == 0:
      p[0] = ('table_ref',p[1])
    else:
      p[0] = ('table_ref',p[1],args)

  def p_join_clause_inner(self, p):
    """join_clause : JOIN
                   | INNER JOIN """
    p[0] = 'join'

  def p_join_clause_cross(self, p):
    """join_clause : CROSS JOIN """
    p[0] = 'cross_join'

  def p_join_clause_left(self, p):
    """join_clause : LEFT JOIN
                   | LEFT OUTER JOIN """
    p[0] = 'left_join'

  def p_join_clause_right(self, p):
    """join_clause : RIGHT JOIN
                   | RIGHT OUTER JOIN """
    p[0] = 'right_join'

  def p_join_clause_natural(self, p):
    """join_clause : NATURAL JOIN """
    p[0] = 'natural_join'

  def p_opt_join_condition_empty(self, p):
    """opt_join_condition : """
    p[0] = {}

  def p_opt_join_condition(self, p):
    """opt_join_condition : join_condition """
    p[0] = p[1]

  def p_join_condition_on(self, p):
    """join_condition : ON expr """
    p[0] = {'on':p[2]}

  def p_join_condition_using(self, p):
    """join_condition : USING '(' ident_list ')' """
    p[0] = {'using':p[3]}

  def p_join_table(self, p):
    """join_table : table_ref join_clause table_factor opt_join_condition """
    if len(p[4]) == 0:
      p[0] = (p[2],p[1],p[3])
    else:
      p[0] = (p[2],p[1],p[3],p[4])

  def p_table_ref_table_factor(self, p):
    """table_ref : table_factor """
    p[0] = p[1]

  def p_table_ref_join_table(self, p):
    """table_ref : join_table """
    p[0] = p[1]

  def p_esc_table_ref(self, p):
    """esc_table_ref : table_ref """
    p[0] = p[1]

  def p_derived_table_list_single(self, p):
    """derived_table_list : esc_table_ref """
    p[0] = [p[1]]

  def p_derived_table_list(self, p):
    """derived_table_list : esc_table_ref ',' derived_table_list """
    p[0] = [p[1]] + p[3]

  def p_join_table_list(self, p):
    """join_table_list : derived_table_list """
    p[0] = p[1]

  def p_where_clause_empty(self, p):
    """where_clause : """
    p[0] = {}

  def p_where_clause(self, p):
    """where_clause : WHERE expr"""
    p[0] = {'where':p[2]}

  def p_group_clause_empty(self, p):
    """group_clause : """
    p[0] = {}

  def p_olap_opt_empty(self, p):
    """olap_opt : """
    p[0] = None

  def p_olap_opt_with_cube(self, p):
    """olap_opt : WITH CUBE """
    p[0] = 'with_cube'

  def p_olap_opt_with_rollup(self, p):
    """olap_opt : WITH ROLLUP """
    p[0] = 'with_rollup'

  def p_group_item(self, p):
    """group_item : order_item """
    p[0] = p[1]

  def p_group_list_single(self, p):
    """group_list : group_item """
    p[0] = [p[1]]

  def p_group_list(self, p):
    """group_list : group_item ',' group_list """
    p[0] = [p[1]] + p[3]

  def p_group_clause(self, p):
    """group_clause : GROUP BY group_list olap_opt """
    if p[4] == None:
      p[0] = {'group_by':p[3]}
    else:
      p[0] = {'group_by':(p[3],p[4])}

  def p_having_clause_empty(self, p):
    """having_clause : """
    p[0] = {}

  def p_having_clause(self, p):
    """having_clause : HAVING expr """
    p[0] = {'having':p[2]}

  def p_select_from(self, p):
    """select_from : FROM join_table_list where_clause group_clause having_clause opt_order_clause opt_limit_clause """
    result = dict({'from':p[2]},**p[3])
    result.update(p[4])
    result.update(p[5])
    result.update(p[6])
    result.update(p[7])
    #result.update(p[8])
    p[0] = result

  def p_select_from_dual(self, p):
    """select_from : FROM DUAL where_clause opt_limit_clause """
    result = dict(p[3],**p[4]) # dual is system table without fields, so equivalent to empty FROM
    p[0] = result

  def p_select_into_from(self, p):
    """select_into : select_from """
    p[0] = p[1]

  def p_select_into_into_select_from(self, p):
    """select_into : into select_from """
    p[0] = dict(p[2],**p[1])

  def p_select_into_select_from_into(self, p):
    """select_into : select_from into """
    p[0] = dict(p[1],**p[2])

  def p_select(self, p):
    """select : SELECT select_options select_item_list select_into """
    args = dict({'location':self._location(p)},**p[2])
    args.update({'items':p[3]})
    args.update(p[4])
    p[0] = [('select',args)]

  def p_variable_assignment_user_var(self, p):
    """variable_assignment : IDENT EQ expr """
    p[0] = ('user',p[1],p[3])

  def p_variable_assignment_global_var(self, p):
    """variable_assignment : GLOBAL IDENT EQ expr """
    p[0] = ('global',p[2],p[4])

  def p_variable_assignment_session_var(self, p):
    """variable_assignment : SESSION IDENT EQ expr """
    p[0] = ('session',p[2],p[4])

  def p_variable_assignments_single(self, p):
    """variable_assignments : variable_assignment """
    p[0] = [p[1]]

  def p_variable_assignments(self, p):
    """variable_assignments : variable_assignment ',' variable_assignments """
    p[0] = [p[1]] + p[3]

  def p_set(self, p):
    """set : SET variable_assignments """
    p[0] = [('set',p[2],{'location':self._location(p)})]

  def p_create_view(self, p):
    """create_view : CREATE VIEW IDENT AS select """
    select = p[5][0][1]
    del select['location']
    p[0] = [('create_view',p[3],select,{'location':self._location(p)})]

  def p_drop_trigger(self, p):
    """drop_trigger : DROP TRIGGER if_exists IDENT """
    p[0] = [('drop_trigger',p[4],dict({'location':self._location(p)},**p[3]))]

  def p_optimize_table(self, p):
    """optimize_table : OPTIMIZE TABLE ident_list """
    p[0] = [('optimize_table',p[3],{'location':self._location(p)})]

  def p_unique_opt_empty(self, p):
    """unique_opt : """
    p[0] = {}

  def p_unique_opt(self, p):
    """unique_opt : UNIQUE """
    p[0] = {'unique':True}

  def p_create_index(self, p):
    """create_index : CREATE unique_opt INDEX IDENT ON IDENT '(' ident_list ')' """
    p[0] = [('add_index',p[6],p[8],dict({'name':p[4],'location':self._location(p)},**p[2]))]

  def p_drop_view(self, p):
    """drop_view : DROP VIEW if_exists ident_list opt_restrict_cascade """
    args = dict({'location':self._location(p)},**p[3])
    args.update(p[5])
    result = []
    for i in p[4]:
      result.append(('drop_view',i,args))
    p[0] = result

  def p_opt_work(self, p):
    """opt_work : WORK
                | """
    pass

  def p_opt_with_consistent_snapshot_empty(self, p):
    """opt_with_consistent_snapshot : """
    p[0] = {}

  def p_opt_with_consistent_snapshot(self, p):
    """opt_with_consistent_snapshot : WITH CONSISTENT SNAPSHOT """
    p[0] = {'consistent_snapshot':True}

  def p_start_transaction(self, p):
    """start_transaction : START TRANSACTION opt_with_consistent_snapshot """
    p[0] = [('start_transaction',dict({'location':self._location(p)},**p[3]))]

  def p_start_transaction_begin(self, p):
    """start_transaction : BEGIN opt_work """
    p[0] = [('start_transaction',{'location':self._location(p)})]

  def p_opt_chain_empty(self, p):
    """opt_chain : """
    p[0] = {}

  def p_opt_chain_no_chain(self, p):
    """opt_chain : AND NO CHAIN """
    p[0] = {'chain':False}

  def p_opt_chain(self, p):
    """opt_chain : AND CHAIN """
    p[0] = {'chain':True}

  def p_opt_release_empty(self, p):
    """opt_release : """
    p[0] = {}

  def p_opt_no_release(self, p):
    """opt_release : NO RELEASE """
    p[0] = {'release':False}

  def p_opt_release(self, p):
    """opt_release : RELEASE """
    p[0] = {'release':True}

  def p_commit(self, p):
    """commit : COMMIT opt_work opt_chain opt_release """
    args = dict({'location':self._location(p)},**p[3])
    args.update(p[4])
    p[0] = [('commit',args)]

  def p_rollback(self, p):
    """rollback : ROLLBACK opt_work opt_chain opt_release """
    args = dict({'location':self._location(p)},**p[3])
    args.update(p[4])
    p[0] = [('rollback',args)]

  def p_statement(self, p):
    """statement : alter_table 
                 | create_table
                 | create_trigger
                 | create_view
                 | create_index
                 | delete
                 | drop_table
                 | drop_trigger
                 | drop_view
                 | insert
                 | optimize_table
                 | rename_table
                 | select
                 | set
                 | update 
                 | start_transaction
                 | commit
                 | rollback """
    p[0] = p[1]

  def p_statements_empty(self, p):
    """statements : """
    p[0] = []

  def p_statements(self, p):
    """statements : statement delim statements"""
    p[0] = p[1] + p[3]

  def p_comments(self, p):
    """comments : COMMENTS """
    pass

  def p_statements_comment(self, p):
    """statements : comments statements"""
    p[0] = p[2]

  def p_delimiter(self, p):
    """delimiter : DELIMITER """
    self._change_delimiter(p[1])

  def p_statements_delimiter(self, p):
    """statements : delimiter statements"""
    p[0] = p[2]

  def error(self,p,i,message):
    location = Location(self.lexer, p.lineno(i), p.lexpos(i))
    raise SQLError(message, location)

  # lex functions ----------------------------------------------------------

  def token(self):
    while True:
      t = self.lexer.token()
      if t == None:
        return t
      if t.type != 'COMMENTS':
        break
    if t.value == self.t_DELIM or t.value == ';' or t.type == 'BEGIN':
      self.last_tokens = []
    else:
      if not (isinstance(t.value,basestring) and len(t.value) == 1 and t.value[0] in self.literals):
        if after(self.last_tokens, ['SELECT']) == 0:
          if t.type != 'COUNT':
            t.type = 'IDENT'
        elif after(self.last_tokens, ['INSERT','(']) >= 0 and self.level == 1 \
          or after(self.last_tokens, ['UPDATE','IDENT','SET']) == 0 \
          or after(self.last_tokens, ['UPDATE','IDENT','SET','IDENT','EQ']) == 0:
          t.type = 'IDENT'
        elif (after(self.last_tokens, ['ALTER','TABLE','CHANGE']) in [0,1] \
          or after(self.last_tokens, ['ALTER','TABLE','MODIFY']) == 0) \
          and t.type != 'COLUMN':
          t.type = 'IDENT'
        elif after(self.last_tokens, ['CREATE', 'TABLE','(']) >= 0:
          if self.last_tokens[len(self.last_tokens)-1] in ['(', ','] and self.level == 1:
            if t.type not in ['CONSTRAINT','PRIMARY','INDEX','KEY','UNIQUE']:
              t.type = 'IDENT'
      else:
        if after(self.last_tokens, ['SELECT']) == 0 and t.type == 'TIMES':
          t.type = '*'
        elif after(self.last_tokens, ['COUNT', '(']) >= 0 and t.type == 'TIMES':
          t.type = '*'
      if t.type != 'DELIMITER':
        if t.type == '(':
          self.level += 1
        elif t.type == ')':
          self.level -= 1
        self.last_tokens.append(t.type)
    print t
    return t

  def column(self,p,i):
    input = self.lexer.lexdata
    last_cr = input.rfind('\n',0,p.lexpos(i))
    if last_cr < 0:
      last_cr = 0
    column = (p.lexpos(i) - last_cr) + 1
    return column

  # private functions ------------------------------------------------------

  def _change_delimiter(self, delim):
    lexretext = self.lexer.lexstateretext[self.lexer.lexstate][0]
    lexretext = lexretext.replace('(?P<t_DELIM>'+self.delim+')',r'(?P<t_DELIM>'+delim+')')
    lexre = re.compile(lexretext, self.lexer.lexreflags | re.VERBOSE)
    self.lexer.lexstateretext[self.lexer.lexstate][0] = lexretext
    self.lexer.lexstatere[self.lexer.lexstate][0] = (lexre, self.lexer.lexstatere[self.lexer.lexstate][0][1])
    self.delim = delim

  def _location(self, p):
    lineno = p.lineno(1)
    file = getattr(self.lexer, 'filename', "<unknown>")
    return '{0}:{1}'.format(file, lineno)

  # public Interface -------------------------------------------------------

  def __init__(self, outputdir=os.path.join(os.path.dirname(__file__)), debug=0):
    self.delim = self.t_DELIM
    self.lexer = lex.lex(object=self,
                         debug=debug,
                         outputdir=outputdir,
                         lextab='sqlplex',
                         optimize=1)
    self.parser = yacc.yacc(start='statements',
                            debug=debug,
                            module=self,
                            outputdir=outputdir,
                            debugfile='sqlp_debug',
                            tabmodule='sqlpyacc',
                            optimize=1)

  def parse(self, sql, filename=None):
    if filename is not None:
      self.lexer.filename = filename
    self.lexer.lineno = 1
    self.lexer.input(sql)
    return self.parser.parse(lexer=self)

  # public Optimizations ---------------------------------------------------

  def optimize(self, stmts):
    stmts = self.join_insertions(stmts)
    stmts = self.join_updates(stmts)
    return stmts

  def table_index(self, stmts):
    dds = [
      'add_column',
      'add_index',
      'add_primary_key',
      'change_column',
      'set_column_default',
      'drop_column_default',
      'remove_column',
      'remove_primary_key',
      'remove_index',
      'rename_table',
      'create_table',
      ]
    index = {}
    globals = {}
    for stmt in stmts:
      if stmt[0] in dds or \
         stmt[0] == 'insert' or \
         stmt[0] == 'delete':
        table_names = [stmt[1]]
      elif stmt[0] == 'select':
        table_names = ['__other'] + self._tables_from_select(stmt[1])
      elif stmt[0] == 'create_view':
        table_names = ['__other'] + self._tables_from_select(stmt[2])
        globals[stmt[1]] = table_names
      elif stmt[0] == 'drop_view':
        table_names = globals.get(stmt[1], ['__other'])
      elif stmt[0] == 'create_trigger':
        table_names = ['__other', stmt[1]]
        globals[stmt[2]] = table_names
      elif stmt[0] == 'drop_trigger':
        table_names = globals.get(stmt[1], ['__other'])
      elif stmt[0] == 'set':
        table_names = ['__other']
      elif stmt[0] == 'update' or \
           stmt[0] == 'optimize' or \
           stmt[0] == 'drop_table':
        table_names = stmt[1]
      for table_name in table_names:
        index.setdefault(table_name,[]).append(stmt)
    return index

  def _from_tables(self, stmt, l):
    if stmt[0] == 'table_ref':
      l.append(stmt[1])
      return l
    elif 'join' in stmt[0]:
      return l + self._from_tables(stmt[1], []) + self._from_tables(stmt[2], [])
    else:
      raise Exception("Not expected 'select' construct")

  def _tables_from_select(self, args):
    result = []
    if 'from' in args:
      for i in args['from']:
        result = result + self._from_tables(i, [])
    return result

  def join_insertions(self, stmts):
    if len(stmts) == 0:
      return []
    optimized = [stmts[0]]
    for stmt in stmts[1:]:
      if stmt[0] == 'insert' and optimized[-1][0] == 'insert' \
          and (stmt[1],stmt[2]) == (optimized[-1][1],optimized[-1][2]):
        for i in stmt[3]:
          optimized[-1][3].append(i)
      else:
        optimized.append(stmt)
    return optimized

  def join_updates(self, stmts):
    if len(all) == 0:
      return []
    optimized = [stmts[0]]
    for stmt in stmts[1:]:
      if stmt[0] == 'update' and optimized[-1][0] == 'update' \
          and (stmt[1],[k for k,v in stmt[2]],stmt[3].get('where',None),stmt[3].get('limit',None)) == \
              (optimized[-1][1],[k for k,v in optimized[-1][2]],optimized[-1][3].get('where',None),optimized[-1][3].get('limit',None)):
        optimized[-1] = stmt
      else:
        optimized.append(stmt)
    return optimized

if __name__ == '__main__':
  from optparse import OptionParser
  import sys, os.path, itertools, glob
  o = OptionParser()
  o.add_option('--regen', action='store_true', dest='regen', default=False,
         help="Regenerate SQL Parser cache")
  o.add_option('--debug', action='store_true', dest='debug', default=False,
         help="Enable debug")
  options, args = o.parse_args()
  files = args[0:] if args else None

  # The only thing special about a regen is that there are no input files.
  if options.regen:
    # Delete the lex/yacc files.  Ply is too stupid to regenerate them
    # properly
    for fileglobs in [os.path.join('./sqlp', f) for f in ["sqlplex.py*", "sqlpyacc.py*"]]:
      for filename in glob.glob(fileglobs):
        os.remove(filename)

  # Instantiate the parser.
  p = SQLParser(debug=options.debug)

  if options.regen:
    sys.exit(0)

  if files == None:
    print >>sys.stderr, "No input files"
    sys.exit(1)

  all = []
  for f in files:
    k = p.parse(open(f).read(), filename=f)
    all = all + k

  #optimized = p.optimize(all)

  #table_index = p.table_index(optimized)

  pprint.pprint(all)

  #print '# Before optimization: ' + str(len(all))
  #print '# After optimization: ' + str(len(optimized))

