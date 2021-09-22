

var util = {};

(function(exports) {

exports.inherits = function(ctor, superCtor) {
  ctor.super_ = superCtor;
  ctor.prototype = Object.create(superCtor.prototype, {
    constructor: {
      value: ctor,
      enumerable: false,
      writable: true,
      configurable: true
    }
  });
};

}(util));



(function() {

		var glsl = {

				compile : function(src, options) {
			var state,
				result,
			    irs
			    ;

				state = new GlslState(options);
			state.setSource(src);

			result = this.preprocessor.process(state);

			if (result) {
				result = this.parser.parse(state);
			}

			if (result) {
				result = this.generate(state);
			}

				if (result) {
				state.status = true;	
			}

				return state;
		},

		target : {
			fragment : 0,
			'x-fragment' : 0,
			'x-shader/x-fragment' : 0,
			vertex : 1,
			'x-vertex' : 1,
			'x-shader/x-vertex' : 1
		}
	};





		var util = {};

		(function(exports) {


			var formatRegExp = /%[sdj%]/g;
	exports.format = function(f) {
	  if (!isString(f)) {
	    var objects = [];
	    for (var i = 0; i < arguments.length; i++) {
	      objects.push(inspect(arguments[i]));
	    }
	    return objects.join(' ');
	  }

		  var i = 1;
	  var args = arguments;
	  var len = args.length;
	  var str = String(f).replace(formatRegExp, function(x) {
	    if (x === '%%') return '%';
	    if (i >= len) return x;
	    switch (x) {
	      case '%s': return String(args[i++]);
	      case '%d': return Number(args[i++]);
	      case '%j':
	        try {
	          return JSON.stringify(args[i++]);
	        } catch (_) {
	          return '[Circular]';
	        }
	      default:
	        return x;
	    }
	  });
	  for (var x = args[i]; i < len; x = args[++i]) {
	      str += ' ' + x;
	  }
	  return str;
	};

		function isString(arg) {
	  return typeof arg === 'string';
	}
	exports.isString = isString;

	exports.inherits = function(ctor, superCtor) {
	  ctor.super_ = superCtor;
	  ctor.prototype = Object.create(superCtor.prototype, {
	    constructor: {
	      value: ctor,
	      enumerable: false,
	      writable: true,
	      configurable: true
	    }
	  });
	};

		}(util));




			function GlslState(options) {
		var i;

			this.options = {
			target : 0,
			language_version : 100,
			opt : {
				fold_constants : true	
			}
		};

			for (i in options) {
			this.options[i] = options[i];	
		}

			this.symbols = new SymbolTable();
		symbol_table_init(this.symbols, options.target);

			this.status = false;
		this.translation_unit = "";
		this.ast = [];
		this.ir = null;

			this.errors = [];
		this.warnings = [];
	}

		proto = GlslState.prototype = {};

	proto.classify_identifier = function(state, name) {
		if (this.symbols.get_variable(name) || this.symbols.get_function(name)) {
			return 'IDENTIFIER';
		} else if (this.symbols.get_type(name)) {
			return 'TYPE_IDENTIFIER';
		} else {
			return 'NEW_IDENTIFIER';
		}
	};

		proto.setSource = function(src) {
		this.src = src;
	};

		proto.getSource = function() {
		return this.src;
	};

		proto.setTranslationUnit = function(tu) {
		this.translation_unit = tu;	
	};

		proto.getTranslationUnit = function() {
		return this.translation_unit;
	};

		proto.addAstNode = function(node) {
		this.ast.push(node);
	};

		proto.getAst = function() {
		return this.ast;
	};

		proto.setIR = function(ir) {
		this.ir = ir;
	};

		proto.getIR = function() {
		return this.ir;
	};

		proto.getStatus = function() {
		return this.status;
	};


	proto.addError = function(msg, line, column) {
		var err;

			if (!line && !column) {
			this.errors.push(msg);
			return;
		}

			err = util.format("%s at line %s, column %s", msg, line, column);

			this.errors.push(err);
	};

	proto.addWarning = function(msg, line, column) {
		var warn;

			if (!line && !column) {
			this.warnings.push(msg);
			return;
		}

			warn = util.format("%s at line %s, column %s", msg, line, column);

			this.warnings.push(warn);
	};

	proto.getErrors = function() {
		return this.errors;
	};

	proto.getWarnings = function() {
		return this.warnings;
	};



	function Preprocessor() {
	}

		Preprocessor.modules = {};

		var proto = Preprocessor.prototype;

		proto.process = function(state) {
		var m,
		    out = state.getSource()
			;

			for (m in Preprocessor.modules) {

				try {
				out = Preprocessor.modules[m].process(out);
			} catch (e) {
				state.addError(e.message, e.lineNumber, e.columnNumber);
				return false;
			}
		}

			state.setTranslationUnit(out);

			return true;
	};

		glsl.preprocessor = new Preprocessor();



		Preprocessor.modules.comments = {

			process : function(src) {
			var i,
			    chr,
			    la,
			    out = "",
			    line = 1,
			    in_single = 0,
			    in_multi = 0
				;

						for (i = 0; i < src.length; i++) {

					chr = src.substr(i, 1);
				la = src.substr(i + 1, 1);

				if (chr == '/' && la == '/' && !in_single && !in_multi) {
					in_single = line;
					i++;
					continue;
				}

				if (chr == "\n" && in_single) {
					in_single = 0;
				}

				if (chr == '/' && la == '*' && !in_multi && !in_single) {
					in_multi = line;
					i++;
					continue;
				}

				if (chr == '*' && la == '/' && in_multi) {

					if (in_multi == line) {
						out += " ";
					}

						in_multi = 0;
					i++;				
					continue;
				}

				if ((!in_multi && !in_single) || chr == "\n") {
					out += 	chr;
					line++;
				}
			}

						return out;
		}

		};



		Preprocessor.modules.directives = {

			state : {
			lines : [],
			defines : {
			}
		},

			process : function(src) {
			var i, l;

						this.state.lines = src.split("\n");
			this.state.defines = {
				GL_ES : '1',
				__FILE__ : '0',
				__LINE__ : '0',
				__VERSION__ : '300'
			};
			this.state.cond_stack = [];

				i = 0;
			l = this.state.lines.length;

				while (i < l) {
				this.state.lines[i] = this.processLine(this.state.lines[i], i);
				i++;
			}

				return this.state.lines.join("\n");
		},

				processLine : function(line, i) {
			var d, matches, raw, e, sub, cond;

				matches = line.match(/^([ \t]*)#(.*)$/);
			if (!matches) {

					if (this.state.cond_stack.length != 0 && !this.state.cond_stack.slice(-1)[0]) {
					return "";
				}

					line = this.processDefines(line, i);

					return line;
			}

				raw = matches[2];

				if (raw.match(/^\s*$/)) {
				return "";
			}

				lmatches = raw.split(/\s+/);

						try {

					switch (lmatches[0]) {

										case 'define':
					case 'undef':
					case 'ifdef':
					case 'endif':
						this[lmatches[0]](line, lmatches);
						return "";
				}

					throw new Error("Invalid directive");

				} catch (e) {

					e.lineNumber = i + 1;
				e.columnNumber = matches[1].length + 1;

						throw e;
			}

			},

			processDefines : function(line, i) {

						this.state.defines.__LINE__ = i + 1;

				for (d in this.state.defines) {
				line = line.split(d).join(this.state.defines[d]);
			}

				return line;
		},

			define : function(line, matches) {

				if (matches.length <= 1 || matches.length > 3) {
				throw new Error("Syntax error in #define");
			}

				this.state.defines[matches[1]] = matches[2] || "";		
		},

			undef : function(line, matches) {

				if (matches.length != 2) {
				throw new Error("Syntax error in #undef");
			}

				delete this.state.defines[matches[1]];
		},

				ifdef : function(line, matches) {

				var def;

				def = !!this.state.defines[matches[1]];

				this.state.cond_stack.push(def);		
		},

			endif : function(line, matches) {

				if (this.state.cond_stack.length) {
				this.state.cond_stack.pop();	
			} else {
				throw new Error("unmatched #endif");
			}
		}

		};



		function Type(name, size, slots, base) {
		this.name = name;
		this.size = size;
		this.slots = slots;
		this.swizzle = (size / slots != 4) ? "xyzw".slice(0, size / slots) : "";
		this.base = base || name;
	}


	Type.castTo = function(val, from, to) {
		var f32;

			switch (to) {

						case 'int':
				return "" + parseInt(val);

				case 'float':
				return "" + parseFloat(val);

				case 'bool':
				return parseFloat(val) ? "1" : "0";
		}

			return val;
	};

	Type.canCast = function(from, to) {
		var t1, t2;

			t1 = types[from];
		t2 = types[to];

			return t1.size === 1 && t2.size === 1;
	};



				var types = {
		_void : new Type("void", 1, 1),
		bool : new Type("bool", 1, 1),
		int : new Type("int", 1, 1),
		float : new Type("float", 1, 1),
		vec2 : new Type("vec2", 2, 1, 'float'),
		vec3 : new Type("vec3", 3, 1, 'float'),
		vec4 : new Type("vec4", 4, 1, 'float'),
		bvec2 : new Type("bvec2", 2, 1, 'bool'),
		bvec3 : new Type("bvec3", 3, 1, 'bool'),
		bvec4 : new Type("bvec4", 4, 1, 'bool'),
		ivec2 : new Type("ivec2", 2, 1, 'int'),
		ivec3 : new Type("ivec3", 3, 1, 'int'),
		ivec4 : new Type("ivec4", 4, 1, 'int'),
		mat2 : new Type("mat2", 4, 2, 'float'),
		mat3 : new Type("mat3", 9, 3, 'float'),
		mat4 : new Type("mat4", 16, 4, 'float'),
		sampler2D : new Type("sampler2D", 1, 1),
		samplerCube : new Type("samplerCube", 1, 1)
	};

	types['void'] = types._void;




	function SymbolTableEntry(name, typedef) {

			this.depth = null;
		this.typedef = typedef;

		this.name = name;

		this.type = null;

		this.base_type = null;

		this.definition = [];	

		this.qualifier = null;

		this.out = name;

		this.constant = null;

		this.size = null;

		this.code = null;
		this.Ast = null;
	}

		SymbolTableEntry.typedef = {
		variable : 0,
		func : 1,
		type : 2
	};

		SymbolTableEntry.prototype.getType = function() {
		return types[this.type];
	};


	function SymbolTable() {
		this.table = {};
		this.depth = 0;
	}

		SymbolTable.prototype = {};
	var proto = SymbolTable.prototype;

	proto.push_scope = function() {
		this.depth++;
	};

	proto.pop_scope = function() {
		var n, t;

				for (n in this.table) {

						if (this.table.hasOwnProperty(n)) {
				t = this.table[n];

								while (t[0] && t[0].depth === this.depth) {
					t.splice(0, 1);	
				}

								if (t.length === 0) {
					delete this.table[n];	
				}
			}
		}

			this.depth--;
	};

	proto.name_declared_this_scope = function(name) {

				var e = this.get_entry(name);

				return e && e.depth === this.depth;
	};

	proto.add_variable = function(name, type) {

				var entry = new SymbolTableEntry(name, SymbolTableEntry.typedef.variable);
		entry.type = type;

				return this._add_entry(entry);
	};

	proto.add_type = function(name, t) {

				var entry = new SymbolTableEntry(name, SymbolTableEntry.typedef.type);
		entry.definition = t;

				return this._add_entry(entry);
	};

	proto.add_function = function(name, type, def) {
		var entry;

			entry = new SymbolTableEntry(name, SymbolTableEntry.typedef.func);
		entry.type = type;

			if (def) {
			entry.definition = def;
		}

			return this._add_entry(entry);
	};

	proto.get_variable = function(name) {

				var entry = this.get_entry(name, SymbolTableEntry.typedef.variable);

			return entry;
	};

	proto.get_type = function(name) {

			var entry = this.get_entry(name, SymbolTableEntry.typedef.type);

			return entry;
	};

	proto.get_function = function(name, def) {

				var entry = this.get_entry(name, SymbolTableEntry.typedef.func, def);

				return entry;
	};

	proto._match_definition = function(def, entry) {

			var i;

				if (!def) {
			return true;	
		}

				if (def.length !== entry.length) {
			return false;	
		}

				for (i = 0; i < def.length; i++) {
			if (def[i] !== entry[i]) {
				return false;
			}
		}

				return true;
	};

	proto._add_entry = function(entry) {

			if (!this.table[entry.name]) {
			this.table[entry.name] = [];	
		}

				this.table[entry.name].splice(0, 0, entry);
		entry.depth = this.depth;

				return entry;
	};

	proto.get_entry = function(name, typedef, def) {

			var t, i, entry;

				t = this.table[name] || [];

		if (def && def.length == 0) {
			def = ["void"];	
		}

			for (i = 0; i < t.length; i++) {

						entry = t[i];

			if (entry.typedef !== typedef) {
				continue;	
			}

			if (typedef !== SymbolTableEntry.typedef.func) {
				return entry;
			}

			if (!def) {
				return entry;
			}

			if (def.join(',') === entry.definition.join(',')) {
				return entry;
			}

			}

				return null;
	};





	function AstNode() {

		this.location = {
			first_line : 0,
			first_column : 0,
			last_line : 0,
			last_column : 0
		};

		this.Dest = null;
		this.Type = null;
		this.Const = false;
	}

		var proto = AstNode.prototype;

	proto.getLocation = function() {
		return this.location;
	};

		proto.setLocation = function(loc) {
		this.location.first_line = loc.first_line;
		this.location.first_column = loc.first_column;
		this.location.last_line = loc.last_line;
		this.location.last_column = loc.last_column;
	};

	proto.toString = function() {
		return this.constructor.name;
	};

		proto.ir = function(state, irs) {
	};


	var ast_operators = [
		"=",
		"POS",
		"NEG",
		"+",
		"-",
		"*",
		"/",
		"%",
		"<<",
		">>",
		"<",
		">",
		"<=",
		">=",
		"==",
		"!=",
		"&",
		"^",
		"|",
		"~",
		"&&",
		"^^",
		"||",
		"!",		
		"*=",
		"/=",
		"%=",
		"+=",
		"-=",
		"<<=",
		">>=",
		"&=",
		"^=",
		"|=",
		"?:",
		"++x",
		"--x",
		"x++",
		"x--",
		".",
		"[]",
		"()",
		"ident",
		"float",
		"int",
		"bool"
	];

		var ast_precision = {
		none : 0,
		highp : 1,
		mediump : 2,
		lowp : 3
	};



	function AstTypeSpecifier(specifier) {
		AstNode.apply(this);
		this.type_specifier = null;
		this.type_name = null;
		this.structure = null;
		this.is_array = 0;
		this.array_size = null;	
		this.precision = 2;
		this.is_precision_statement = null;

			if (AstTypeSpecifier[typeof specifier]) {
			AstTypeSpecifier[typeof specifier].call(this, specifier);
		}
	}

		util.inherits(AstTypeSpecifier, AstNode);
	proto = AstTypeSpecifier.prototype;

	AstTypeSpecifier.number = function(specifier) {
		this.type_specifier = specifier;
		this.precision = ast_precision.none;
		this.is_precision_statement = false;
		this.type_name = types[specifier].name;
	};

		AstTypeSpecifier.string = function(name) {
		this.type_specifier = types[name];
		this.type_name = name;
		this.is_array = false;
		this.precision = ast_precision.none;
		this.is_precision_statement = false;
	};

		AstTypeSpecifier.object = function(s) {
		this.type_specifier = types.struct;
		this.type_name = s.name;
		this.structure = s;
		this.is_array = false;
		this.precision = ast_precision.none;
		this.is_precision_statement = false;			
	};

	proto.toString = function() {
		var i, prec;

				if (this.is_precision_statement) {

						for (i in ast_precision) {
				if (ast_precision.hasOwnProperty(i) && ast_precision[i] === this.precision) {
					prec = i;
					break;
				}
			}

						return util.format("precision %s %s;\n", prec, this.type_name);
		}

				return (this.type_specifier === types.struct ? this.structure : this.type_name)
		    + (this.is_array ? util.format("[%s]", this.array_size || "") : "")
			;
	};


	function AstFunction() {
		AstNode.apply(this);

			this.return_type = null;
		this.identifier = null;
		this.parameters = [];

			this.entry = null;
	}

		util.inherits(AstFunction, AstNode);
	proto = AstFunction.prototype;

	proto.toString = function() {
		return util.format("%s %s(%s)", this.return_type, this.identifier, this.parameters.join(", "));			
	};

	function AstExpression(oper, ex0, ex1, ex2) {
		AstNode.apply(this);

			this.oper = oper;
		this.grouped = false;
		this.subexpressions = [null, null, null];
		this.primary_expression = {};
		this.expressions = [];

			if (ast_operators.indexOf(this.oper) === -1) {
			this.oper = 'ident';
			this.primary_expression.identifier = oper;
		} else {
			this.subexpressions[0] = ex0;
			this.subexpressions[1] = ex1;
			this.subexpressions[2] = ex2;
		}
	}

		util.inherits(AstExpression, AstNode);
	proto = AstExpression.prototype;

	proto.toString = function() {

			var output;

			switch (this.oper) {
			case '=':
			case '+':
			case '-':
			case '*':
			case '/':
			case '%':
			case "<<":
			case ">>":
			case "<":
			case ">":
			case "<=":
			case ">=":
			case "==":
			case "!=":
			case "&":
			case "^":
			case "|":
			case "~":
			case "&&":
			case "^^":
			case "||":
			case '*=':
			case '/=':
			case '%=':
			case '+=':
			case '-=':
			case '<<=':
			case '>>=':
			case '&=':
			case '^=':
			case '|=':
				output = util.format("%s %s %s", this.subexpressions[0], this.oper, this.subexpressions[1]);
				break;

				case '.':
				output = util.format("%s.%s", this.subexpressions[0], this.primary_expression.identifier);
				break;

				case 'POS':
				output = util.format("+%s", this.subexpressions[0]);
				break;
			case 'NEG':
				output = util.format("-%s", this.subexpressions[0]);			
				break;

				case '~':
			case '!':
				output = util.format("%s%s", this.oper, this.subexpressions[0]);			
				break;

				case '++x':
			case '--x':
				output = util.format("%s%s", this.oper.replace('x', ''), this.subexpressions[0]);
				break;

						case 'x++':
			case 'x--':
				output = util.format("%s%s", this.subexpressions[0], this.oper.replace('x', '') );
				break;

				case '?:':
				output = util.format("%s ? %s : %s", this.subexpressions[0], this.subexpressions[1], this.subexpressions[2]);				
				break;

				case '[]':
				output = util.format("%s[%s]", this.subexpressions[0], this.subexpressions[1]);				
				break;

				case '()':
				output = util.format("%s(%s)", this.subexpressions[0], this.expressions.join(", "));
				break;

				case 'ident':
				output = util.format("%s", this.primary_expression.identifier);
				break;

						case 'float':
				output = util.format("%s", this.primary_expression.float_constant);
				break;

						case 'int':
				output = util.format("%s", this.primary_expression.int_constant);
				break;

						case 'bool':
				output = util.format("%s", this.primary_expression.bool_constant ? 'true' : 'false');
				break;

			}

			return this.grouped ? util.format("(%s)", output ) : output;
	};

	function AstFullySpecifiedType() {
		AstNode.apply(this);

				this.qualifier = [];
		this.specifier = null;
	}

		util.inherits(AstFullySpecifiedType, AstNode);
	proto = AstFullySpecifiedType.prototype;

	proto.toString = function() {
		var output;

			output = this.qualifier.slice(0);
		output.push(this.specifier);

			return output.join(' ');
	};

	function AstDeclaration(identifier, is_array, array_size, initializer) {
		AstNode.apply(this);

			this.identifier = identifier;
		this.is_array = is_array;
		this.array_size = array_size;
		this.initializer = initializer;
	}

		util.inherits(AstDeclaration, AstNode);
	proto = AstDeclaration.prototype;

	proto.toString = function() {
		return this.identifier
			+ (this.is_array ? util.format("[%s]", this.array_size === undefined ? '' : this.array_size) : '')
			+ (this.initializer ? util.format(" = %s", this.initializer) : "")
			;
	};


	function AstDeclaratorList(type) {
		AstNode.apply(this);

			this.type = type;
		this.declarations = [];
		this.invariant = 0;
	}

		util.inherits(AstDeclaratorList, AstNode);
	proto = AstDeclaratorList.prototype;

	proto.toString = function() {
		return util.format("%s %s;\n", this.type || "invariant ", this.declarations.join(", "));
	};


	function AstParameterDeclarator() {
		AstNode.apply(this);
		this.type = null;
		this.identifier = null;
		this.is_array = false;
		this.array_size = 0;
		this.formal_parameter = null;
		this.is_void = null;
	}

		util.inherits(AstParameterDeclarator, AstNode);
	proto = AstParameterDeclarator.prototype;

	proto.toString = function() {
		return this.type
		    + (this.identifier ? " " + this.identifier : "")
			+ (this.is_array ? util.format("[%s]", this.array_size) : "")
			;
	};


	function AstExpressionStatement(ex) {
		AstNode.apply(this);

			this.expression = ex;
	}

		util.inherits(AstExpressionStatement, AstNode);
	proto = AstExpressionStatement.prototype;

	proto.toString = function() {
		return util.format("%s;\n", this.expression || "");
	};


	function AstCompoundStatement(new_scope, statements) {
		AstNode.apply(this);
		this.new_scope = new_scope;
		if (statements) {
			this.statements = statements;
		} else {
			this.statements = [];
		}
	}

		util.inherits(AstCompoundStatement, AstNode);
	proto = AstCompoundStatement.prototype;

	proto.toString = function() {
		var str,
			stmts,
		    indent
			;

			AstCompoundStatement._depth++;
		indent = new Array(AstCompoundStatement._depth).join("  ")

			stmts = indent + "  " + this.statements.join(indent + "  ");

			str = "\n" + indent + "{\n"
		    + stmts
			+ indent + "}\n"
			;

			AstCompoundStatement._depth--;

			return str;
	};

	AstCompoundStatement._depth = 0;

	function AstFunctionDefinition() {
		AstNode.apply(this);

			this.proto_type = null;
		this.body = null;
	}

		util.inherits(AstFunctionDefinition, AstNode);
	proto = AstFunctionDefinition.prototype;

	proto.toString = function() {
		return util.format("%s %s", this.proto_type, this.body);
	};

	function AstExpressionBin(oper, ex0, ex1) {
		AstExpression.apply(this, [oper, ex0, ex1]);
	}

		util.inherits(AstExpressionBin, AstExpression);
	proto = AstExpressionBin.prototype;

	function AstFunctionExpression(arg) {
		AstExpression.apply(this);
		this.cons = false;

			if (arg.constructor.name === 'AstExpression') {
			this.cons = false;
			AstExpression.call(this, '()', arg);
		} else if (arg.constructor.name === 'AstTypeSpecifier') {
			this.cons = true;
			AstExpression.call(this, '()', arg);
		}

		}

		util.inherits(AstFunctionExpression, AstExpression);
	proto = AstFunctionExpression.prototype;

		proto.is_constructor = function() {
		return this.cons;
	};

		proto.toString = function() {
		return util.format("%s(%s)", this.subexpressions[0], this.expressions.join(", "));
	};



	function AstSelectionStatement(condition, then_statement, else_statement) {
		AstNode.apply(this);
		this.condition = condition;
		this.then_statement = then_statement;
		this.else_statement = else_statement;
	}

		util.inherits(AstSelectionStatement, AstNode);
	proto = AstSelectionStatement.prototype;

	proto.toString = function() {
		return util.format("if (%s) %s %s", this.condition, this.then_statement, this.else_statement ? util.format("else %s", this.else_statement) : "");
	};


	function AstStructSpecifier(identifier, declarator_list) {
		AstNode.apply(this);
		this.name = null;
		this.declarations = [];

			if (identifier === null) {
			identifier = util.format("#anon_struct%d", AstStructSpecifier.anon_count);
			AstStructSpecifier.anon_count++;
		}
		this.name = identifier;
		this.declarations = declarator_list.declarations;
	}

		AstStructSpecifier.anon_count = 1;

		util.inherits(AstStructSpecifier, AstNode);
	proto = AstStructSpecifier.prototype;



	function AstJumpStatement(mode, return_value) {
		AstNode.apply(this);

			this.opt_return_value = null;
		this.mode = mode;

			if (mode === 'return') {
			this.opt_return_value = return_value;	
		}	
	}

		util.inherits(AstJumpStatement, AstNode);
	proto = AstJumpStatement.prototype;

	proto.toString = function() {

				switch (this.mode) {

				case 'continue':
			case 'break':
			case 'discard':
			case 'debugger':
				return this.mode + ";\n";

						case 'return':
				return util.format("return%s;\n", this.opt_return_value ? " " + this.opt_return_value : "");
		}
	};


			glsl.ast = {
		Node : AstNode,
		TypeSpecifier : AstTypeSpecifier,
		Function : AstFunction,
		Expression : AstExpression,	
		FullySpecifiedType : AstFullySpecifiedType,
		Declaration : AstDeclaration,
		DeclaratorList : AstDeclaratorList,
		ParameterDeclarator : AstParameterDeclarator,
		ExpressionStatement : AstExpressionStatement,
		CompoundStatement : AstCompoundStatement,
		FunctionDefinition : AstFunctionDefinition,
		ExpressionBin : AstExpressionBin,
		FunctionExpression : AstFunctionExpression,
		SelectionStatement : AstSelectionStatement,
		StructSpecifier : AstStructSpecifier,
		JumpStatement : AstJumpStatement	
	};



		var builtin = {

				vars : {

					vertex : [
				{
					position : 0,
					type : 'vec4',
					name : 'gl_Position',
					out : 'result@0'
				},
				{
					position : 1,
					type : 'float',
					name : 'gl_PointSize',
					out : 'result@1'
				}
			],

				fragment : [
				{
					position : 0,
					type : 'vec4',
					name : 'gl_FragColor',
					out : 'result@0'
				}
			]
		},

		oper : {
			"!" : {
				"bool:bool" : [
					"SEQ %1.x %2.x 0.0"
					]
				},
			"+" : {
				"int,int:int" : ["ADD %1.x %2.x %3.x"],
				"float,float:float" : ["ADD %1.x %2.x %3.x"],
				"float,vec2:vec2" : ["ADD %1.xy %2.x %3.xy"],
				"float,vec3:vec3" : ["ADD %1.xyz %2.x %3.xyz"],
				"float,vec4:vec4" : ["ADD %1 %2.x %3"],
				"vec2,float:vec2" : ["ADD %1.xy %2.xy %3.x"],
				"vec3,float:vec3" : ["ADD %1.xyz %2.xyz %3.x"],
				"vec4,float:vec4" : ["ADD %1 %2 %3.x"],
				"vec2,vec2:vec2" : ["ADD %1.xy %2.xy %3.xy"],
				"vec3,vec3:vec3" : ["ADD %1.xyz %2.xyz %3.xyz"],
				"vec4,vec4:vec4" : ["ADD %1 %2 %3"]
				},
			"-" : {
				"int,int:int" : ["SUB %1.x %2.x %3.x"],
				"float,float:float" : ["SUB %1.x %2.x %3.x"],
				"float,vec2:vec2" : ["SUB %1.xy %2.x %3.xy"],
				"float,vec3:vec3" : ["SUB %1.xyz %2.x %3.xyz"],
				"float,vec4:vec4" : ["SUB %1 %2.x %3"],
				"vec2,float:vec2" : ["SUB %1.xy %2.xy %3.x"],
				"vec3,float:vec3" : ["SUB %1.xyz %2.xyz %3.x"],
				"vec4,float:vec4" : ["SUB %1 %2 %3.x"],
				"vec2,vec2:vec2" : ["SUB %1.xy %2.xy %3.xy"],
				"vec3,vec3:vec3" : ["SUB %1.xyz %2.xyz %3.xyz"],
				"vec4,vec4:vec4" : ["SUB %1 %2 %3"]
			},
			"*" : {
				"int,int:int" : ["MUL %1.x %2.x %3.x"],
				"float,float:float" : ["MUL %1.x %2.x %3.x"],
				"float,vec2:vec2" : ["MUL %1.xy %2.x %3.xy"],
				"float,vec3:vec3" : ["MUL %1.xyz %2.x %3.xyz"],
				"float,vec4:vec4" : ["MUL %1 %2.x %3"],
				"vec2,float:vec2" : ["MUL %1.xy %2.xy %3.x"],
				"vec3,float:vec3" : ["MUL %1.xyz %2.xyz %3.x"],
				"vec4,float:vec4" : ["MUL %1 %2 %3.x"],
				"vec2,vec2:vec2" : ["MUL %1.xy %2.xy %3.xy"],
				"vec3,vec3:vec3" : ["MUL %1.xyz %2.xyz %3.xyz"],
				"vec4,vec4:vec4" : ["MUL %1 %2 %3"],
				"mat3,vec3:vec3" : [
					"MUL %1.xyz %2.xyz %3.x",
					"MAD %1.xyz %2@1.xyz %3.y %1",
					"MAD %1.xyz %2@2.xyz %3.z %1"
					],
				"mat4,vec4:vec4" : [
					"MUL %1 %2 %3.x",
					"MAD %1 %2@1 %3.y %1",
					"MAD %1 %2@2 %3.z %1",
					"MAD %1 %2@3 %3.w %1"
					],
				"mat4,mat4:mat4" : [
					"MUL %1 %2 %3.x",
					"MAD %1 %2@1 %3.y %1",
					"MAD %1 %2@2 %3.z %1",
					"MAD %1 %2@3 %3.w %1",
					"MUL %1@1 %2 %3@1.x",
					"MAD %1@1 %2@1 %3@1.y %1@1",
					"MAD %1@1 %2@2 %3@1.z %1@1",
					"MAD %1@1 %2@3 %3@1.w %1@1",
					"MUL %1@2 %2 %3@2.x",
					"MAD %1@2 %2@1 %3@2.y %1@2",
					"MAD %1@2 %2@2 %3@2.z %1@2",
					"MAD %1@2 %2@3 %3@2.w %1@2",
					"MUL %1@3 %2 %3@3.x",
					"MAD %1@3 %2@1 %3@3.y %1@3",
					"MAD %1@3 %2@2 %3@3.z %1@3",
					"MAD %1@3 %2@3 %3@3.w %1@3"
					]
				},
			"/" : {
				"int,int:int" : ["DIV %1.x %2.x %3.x"],
				"float,float:float" : ["DIV %1.x %2.x %3.x"],
				"float,vec2:vec2" : ["DIV %1.xy %2.x %3.xy"],
				"float,vec3:vec3" : ["DIV %1.xyz %2.x %3.xyz"],
				"float,vec4:vec4" : ["DIV %1 %2.x %3"],
				"vec2,float:vec2" : ["DIV %1.xy %2.xy %3.x"],
				"vec3,float:vec3" : ["DIV %1.xyz %2.xyz %3.x"],
				"vec4,float:vec4" : ["DIV %1 %2 %3.x"],
				"vec2,vec2:vec2" : ["DIV %1.xy %2.xy %3.xy"],
				"vec3,vec3:vec3" : ["DIV %1.xyz %2.xyz %3.xyz"],
				"vec4,vec4:vec4" : ["DIV %1 %2 %3"]
				},
			"<" : {
				"int,int:bool" : ["SLT %1.x %2.x %3.x"],
				"float,float:bool" : ["SLT %1.x %2.x %3.x"]
				},
			">" : {
				"int,int:bool" : ["SGT %1.x %2.x %3.x"],
				"float,float:bool" : ["SGT %1.x %2.x %3.x"]
				},
			"<=" : {
				"int,int:bool" : ["SLE %1.x %2.x %3.x"],
				"float,float:bool" : ["SLE %1.x %2.x %3.x"]
				},
			">=" : {
				"int,int:bool" : ["SGE %1.x %2.x %3.x"],
				"float,float:bool" : ["SGE %1.x %2.x %3.x"]
				},
			"==" : {
				"int,int:bool" : ["SEQ %1.x %2.x %3.x"],
				"float,float:bool" : ["SEQ %1.x %2.x %3.x"]
				},
			"!=" : {
				"int,int:bool" : ["SNE %1.x %2.x %3.x"],
				"float,float:bool" : ["SNE %1.x %2.x %3.x"]
				},
			"&&" : {
				"bool,bool:bool" : [
					"AND %1.x %2.x %3.x",
					"AND %1.x %1.x 1"
					]
				},
			"^^" : {
				"bool,bool:bool" : [
					"XOR %1.x %2.x %3.x",
					"AND %1.x %1.x 1"
					]
				},
			"||" : {
				"bool,bool:bool" : [
					"OR %1.x %2.x %3.x",
					"AND %1.x %1.x 1"
					]
				}
		},

		func : {
			"abs": {
				"float:float" : ["ABS %1.x %2.x"],
				"vec2:vec2" : ["ABS %1.xy %2.xy"],
				"vec3:vec3" : ["ABS %1.xyz %2.xyz"],
				"vec4:vec4" : ["ABS %1 %2"],
				},
			"ceil": {
				"float:float" : ["CEIL %1.x %2.x"],
				"vec2:vec2" : ["CEIL %1.xy %2.xy"],
				"vec3:vec3" : ["CEIL %1.xyz %2.xyz"],
				"vec4:vec4" : ["CEIL %1 %2"],
				},
			"clamp": {
				"float,float,float:float" : [
					"MAX %1.x %2.x %3.x",
					"MIN %1.x %1.x %4.x"
					],
				"vec2,float,float:vec2" : [
					"MAX %1.xy %2.xy %3.x",
					"MIN %1.xy %1.xy %4.x"
					],
				"vec3,float,float:vec3" : [
					"MAX %1.xyz %2.xyz %3.x",
					"MIN %1.xyz %1.xyz %4.x"
					],
				"vec4,float,float:vec4" : [
					"MAX %1 %2 %3.x",
					"MIN %1 %1 %4.x"
					],
				"vec2,vec2,vec2:vec2" : [
					"MAX %1.xy %2.xy %3.xy",
					"MIN %1.xy %1.xy %4.xy"
					],
				"vec3,vec3,vec3:vec3" : [
					"MAX %1.xyz %2.xyz %3.xyz",
					"MIN %1.xyz %1.xyz %4.xyz"
					],
				"vec4,vec4,vec4:vec4" : [
					"MAX %1 %2 %3",
					"MIN %1 %1 %4"
					]
				},
			"cos": {
				"float:float" : ["COS %1.x %2.x"],
				"vec2:vec2" : ["COS %1.xy %2.xy"],
				"vec3:vec3" : ["COS %1.xyz %2.xyz"],
				"vec4:vec4" : ["COS %1 %2"],
				},
			"degrees": {
				"float:float" : ["MUL %1.x %2.x " + (180 / Math.PI)],
				"vec2:vec2" : ["MUL %1.xy %2.xy " + (180 / Math.PI)],
				"vec3:vec3" : ["MUL %1.xyz %2.xyz " + (180 / Math.PI)],
				"vec4:vec4" : ["MUL %1 %2 " + (180 / Math.PI)]
				},
			"dot": {
				"vec2,vec2:float" : ["DP2 %1.x %2.xy %3.xy"],
				"vec3,vec3:float" : ["DP3 %1.x %2.xyz %3.xyz"],
				"vec4,vec4:float" : ["DP4 %1.x %2 %3"]
				},
			"floor": {
				"float:float" : ["FLR %1.x %2.x"],
				"vec2:vec2" : ["FLR %1.xy %2.xy"],
				"vec3:vec3" : ["FLR %1.xyz %2.xyz"],
				"vec4:vec4" : ["FLR %1 %2"],
				},
			"fract": {
				"float:float" : ["FRC %1.x %2.x"],
				"vec2:vec2" : ["FRC %1.xy %2.xy"],
				"vec3:vec3" : ["FRC %1.xyz %2.xyz"],
				"vec4:vec4" : ["FRC %1 %2"],
				},
	        "max": {
				"float,float:float" : ["MAX %1.x %2.x %3.x"],
				"vec2,float:vec2" : ["MAX %1.xy %2.xy %3.x"],
				"vec3,float:vec3" : ["MAX %1.xyz %2.xyz %3.x"],
				"vec4,float:vec4" : ["MAX %1 %2 %3.x"],
				"vec2,vec2:vec2" : ["MAX %1.xy %2.xy %3.xy"],
				"vec3,vec3:vec3" : ["MAX %1.xyz %2.xyz %3.xyz"],
				"vec4,vec4:vec4" : ["MAX %1 %2 %3"]
				},
	        "min": {
				"float,float:float" : ["MIN %1.x %2.x %3.x"],
				"vec2,float:vec2" : ["MIN %1.xy %2.xy %3.x"],
				"vec3,float:vec3" : ["MIN %1.xyz %2.xyz %3.x"],
				"vec4,float:vec4" : ["MIN %1 %2 %3.x"],
				"vec2,vec2:vec2" : ["MIN %1.xy %2.xy %3.xy"],
				"vec3,vec3:vec3" : ["MIN %1.xyz %2.xyz %3.xyz"],
				"vec4,vec4:vec4" : ["MIN %1 %2 %3"]
				},
	        "mix": {
				"float,float,float:float" : [
					"MAD %1.x -%2.x %4.x %2.x",
					"MAD %1.x %3.x %4.x %1.x"
					],
				"vec2,vec2,float:vec2" : [
					"MAD %1.xy -%2.xy %4.x %2.xy",
					"MAD %1.xy %3.xy %4.x %1.xy"
					],
				"vec3,vec3,float:vec3" : [
					"MAD %1.xyz -%2.xyz %4.x %2.xyz",
					"MAD %1.xyz %3.xyz %4.x %1.xyz"
					],
				"vec4,vec4,float:vec4" : [
					"MAD %1 -%2 %4.x %2",
					"MAD %1 %3 %4.x %1"
					],
				"vec2,vec2,vec2:vec2" : [
					"MAD %1.xy -%2.xy %4.xy %2.xy",
					"MAD %1.xy %3.xy %4.xy %1.xy"
					],
				"vec3,vec3,vec3:vec3" : [
					"MAD %1.xyz -%2.xyz %4.xyz %2.xyz",
					"MAD %1.xyz %3.xyz %4.xyz %1.xyz"
					],
				"vec4,vec4,vec4:vec4" : [
					"MAD %1 -%2 %4 %2",
					"MAD %1 %3 %4 %1"
					]
				},
	        "mod": {
				"float,float:float" : ["MOD %1.x %2.x %3.x"],
				"vec2,float:vec2" : ["MOD %1.xy %2.xy %3.x"],
				"vec3,float:vec3" : ["MOD %1.xyz %2.xyz %3.x"],
				"vec4,float:vec4" : ["MOD %1 %2 %3.x"],
				"vec2,vec2:vec2" : ["MOD %1.xy %2.xy %3.xy"],
				"vec3,vec3:vec3" : ["MOD %1.xyz %2.xyz %3.xyz"],
				"vec4,vec4:vec4" : ["MOD %1 %2 %3"]
				},
	        "normalize": {
				"vec3:vec3" : [
					"DP3 %1.x %2 %2",
					"RSQ %1.x %1.x",
					"MUL %1.xyz %2.xyz %1.x"
					],
				"vec4:vec4" : [
					"DP4 %1.x %2 %2",
					"RSQ %1.x %1.x",
					"MUL %1 %2 %1.x"
					]
				},
			"pow": {
				"float,float:float" : ["POW %1.x %2.x %3.x"]
				},
	        "reflect": {
				"vec3,vec3:vec3" : [
					"DP3 %1.x %3 %2",
					"MUL %1.xyz %3 %1.x",
					"MAD %1.xyz -%1 2.0 %2"
					]
				},
			"radians": {
				"float:float" : ["MUL %1.x %2.x " + (Math.PI / 180)],
				"vec2:vec2" : ["MUL %1.xy %2.xy " + (Math.PI / 180)],
				"vec3:vec3" : ["MUL %1.xyz %2.xyz " + (Math.PI / 180)],
				"vec4:vec4" : ["MUL %1 %2 " + (Math.PI / 180)],
				},
			"sign": {
				"float:float" : [
					"SGT %t1.x %2.x 0",
					"SLT %t1.y %2.x 0",
					"ADD %1.x %t1.x -%t1.y"
					],
				"vec2:vec2" : [
					"SGT %t1.xy %2.xy 0",
					"SLT %t1.zw %2.zw 0",
					"ADD %1.xy %t1.xy -%t1.zw"
					],
				"vec3:vec3" : [
					"SGT %t1.xyz %2.xyz 0",
					"SLT %t2.xyz %2.xyz 0",
					"ADD %1.xyz %t1.xyz -%t2.xyz"
					],
				"vec4:vec4" : [
					"SGT %t1 %2 0",
					"SLT %t2 %2 0",
					"ADD %1 %t1 -%t2"
					],
				},
			"sin": {
				"float:float" : ["SIN %1.x %2.x"],
				"vec2:vec2" : ["SIN %1.xy %2.xy"],
				"vec3:vec3" : ["SIN %1.xyz %2.xyz"],
				"vec4:vec4" : ["SIN %1 %2"],
				},
			"step": {
				"float,float:float" : ["SGE %1.x %3.x %2.x"],
				"float,vec2:vec2" : ["SGE %1.xy %3.x %2.xy"],
				"float,vec3:vec3" : ["SGE %1.xyz %3.x %2.xyz"],
				"float,vec4:vec4" : ["SGE %1 %3.x %2"],
				"vec2,vec2:vec2" : ["SGE %1.xy %3.xy %2.xy"],
				"vec3,vec3:vec3" : ["SGE %1.xyz %3.xyz %2.xyz"],
				"vec4,vec4:vec4" : ["SGE %1 %3 %3"],
				},
			"tan": {
				"float:float" : ["TAN %1.x %2.x"],
				"vec2:vec2" : ["TAN %1.xy %2.xy"],
				"vec3:vec3" : ["TAN %1.xyz %2.xyz"],
				"vec4:vec4" : ["TAN %1 %2"],
				},
			"texture2D": {
				"sampler2D,vec2:vec4" : ["TEX %1 %3 %2 \"2D\""]
				}
		}
	};

		function _builtinParseType(str) {
		var parts, ret;

			parts = str.split(":");
		parts[0] = parts[0].split(",");

				ret = {
			src : parts[0],
			dest : parts[1]
		};

				return ret;
	}


			function symbol_table_init(table, target) {
		var i, j, vars, v, entry, types, name;

			vars = (target === glsl.target.vertex) ? builtin.vars.vertex : builtin.vars.fragment;

			for (i = 0; i < vars.length; i++) {
			v = vars[i];
			entry = table.add_variable(v.name, v.type);
			entry.position = v.position;
			entry.out = v.out;
		}

			vars = builtin.func;

			for (name in vars) {
			v = vars[name];
			for (j in v) {
				types = _builtinParseType(j);	
				entry = table.add_function(name, types.dest, types.src);
				entry.code = v[j]
			}
		}
	}


	var parser = (function(){
	var o=function(k,v,o,l){for(o=o||{},l=k.length;l--;o[k[l]]=v);return o},$V0=[13,14,15,16,17,21,22,47,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$V1=[1,18],$V2=[1,19],$V3=[1,20],$V4=[1,21],$V5=[1,22],$V6=[1,53],$V7=[1,54],$V8=[1,17],$V9=[1,44],$Va=[1,45],$Vb=[1,28],$Vc=[1,47],$Vd=[1,48],$Ve=[1,49],$Vf=[1,50],$Vg=[1,40],$Vh=[1,41],$Vi=[1,42],$Vj=[1,43],$Vk=[1,46],$Vl=[1,55],$Vm=[1,56],$Vn=[1,57],$Vo=[1,58],$Vp=[1,59],$Vq=[1,60],$Vr=[1,61],$Vs=[1,62],$Vt=[1,63],$Vu=[1,64],$Vv=[1,65],$Vw=[1,66],$Vx=[1,67],$Vy=[1,68],$Vz=[1,69],$VA=[1,70],$VB=[1,71],$VC=[1,72],$VD=[1,73],$VE=[1,74],$VF=[1,75],$VG=[1,76],$VH=[1,37],$VI=[1,38],$VJ=[1,39],$VK=[1,77],$VL=[5,13,14,15,16,17,21,47,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$VM=[1,82],$VN=[1,83],$VO=[1,84],$VP=[1,86],$VQ=[1,87],$VR=[49,106],$VS=[21,47,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$VT=[2,121],$VU=[1,101],$VV=[1,102],$VW=[1,103],$VX=[1,100],$VY=[2,159],$VZ=[21,25,26,49,106],$V_=[2,141],$V$=[21,25,26,30,32,49,106],$V01=[21,47,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,169],$V11=[21,47,120,121,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$V21=[21,25,26,30,32,34,49,106],$V31=[2,177],$V41=[2,12],$V51=[11,23,30,32,34,36,38,39,40,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,92,93,94,95,96,97,98,99,100,101,102,106,170],$V61=[5,10,13,14,15,16,17,21,25,26,28,29,30,39,40,47,51,57,58,59,60,106,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,172,189,191,193,194,195,196,197,198,202,203,204,205,206],$V71=[1,169],$V81=[1,170],$V91=[1,171],$Va1=[1,172],$Vb1=[1,156],$Vc1=[1,157],$Vd1=[1,182],$Ve1=[1,163],$Vf1=[1,164],$Vg1=[1,165],$Vh1=[1,166],$Vi1=[1,136],$Vj1=[1,127],$Vk1=[1,138],$Vl1=[1,139],$Vm1=[1,140],$Vn1=[1,141],$Vo1=[1,142],$Vp1=[1,143],$Vq1=[1,144],$Vr1=[1,145],$Vs1=[1,146],$Vt1=[1,147],$Vu1=[1,148],$Vv1=[1,149],$Vw1=[32,49],$Vx1=[10,21,25,26,28,29,30,39,40,47,51,57,58,59,60,106,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,172,189,193,194,195,196,197,198,202,203,204,205,206],$Vy1=[10,21,25,26,28,29,30,39,40,47,51,57,58,59,60,106,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,172,189,191,193,194,195,196,197,198,202,203,204,205,206],$Vz1=[1,216],$VA1=[23,32,36,49,106],$VB1=[23,32,36,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,106],$VC1=[2,58],$VD1=[23,32,36,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,92,93,94,95,96,97,98,99,100,101,102,106],$VE1=[1,251],$VF1=[23,32,36,49,88,90,106],$VG1=[1,252],$VH1=[23,32,34,36,38,39,40,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,92,93,94,95,96,97,98,99,100,101,102,106],$VI1=[10,21,25,26,28,29,30,39,40,47,51,57,58,59,60,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$VJ1=[23,32,36,49,86,88,90,106],$VK1=[1,253],$VL1=[23,32,36,49,84,86,88,90,106],$VM1=[1,256],$VN1=[23,32,36,49,82,84,86,88,90,106],$VO1=[1,257],$VP1=[23,32,36,49,80,82,84,86,88,90,106],$VQ1=[1,261],$VR1=[23,32,36,49,78,80,82,84,86,88,90,106],$VS1=[1,264],$VT1=[1,265],$VU1=[10,21,25,26,28,29,30,32,39,40,47,51,57,58,59,60,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$VV1=[23,32,36,49,75,76,78,80,82,84,86,88,90,106],$VW1=[1,266],$VX1=[1,267],$VY1=[1,268],$VZ1=[1,269],$V_1=[23,32,36,49,70,71,72,73,75,76,78,80,82,84,86,88,90,106],$V$1=[1,270],$V02=[1,271],$V12=[23,32,36,49,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,106],$V22=[1,272],$V32=[1,273],$V42=[23,32,36,49,57,58,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,106],$V52=[1,274],$V62=[1,275],$V72=[1,276],$V82=[21,47,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,172],$V92=[1,306],$Va2=[30,34],$Vb2=[32,106],$Vc2=[10,21,25,26,28,29,30,39,40,47,51,57,58,59,60,106,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169];
	var parser = {trace: function trace() { },
	yy: {},
	symbols_: {"error":2,"glsl-start":3,"translation_unit":4,"EOF":5,"version_statement":6,"extension_statement_list":7,"external_declaration_list":8,"VERSION":9,"INTCONSTANT":10,"EOL":11,"pragma_statement":12,"PRAGMA_DEBUG_ON":13,"PRAGMA_DEBUG_OFF":14,"PRAGMA_OPTIMIZE_ON":15,"PRAGMA_OPTIMIZE_OFF":16,"PRAGMA_INVARIANT_ALL":17,"extension_statement":18,"any_identifier":19,"variable_identifier":20,"TYPE_IDENTIFIER":21,"EXTENSION":22,":":23,"external_declaration":24,"IDENTIFIER":25,"NEW_IDENTIFIER":26,"primary_expression":27,"FLOATCONSTANT":28,"BOOLCONSTANT":29,"(":30,"expression":31,")":32,"postfix_expression":33,"[":34,"integer_expression":35,"]":36,"function_call":37,".":38,"++":39,"--":40,"function_call_or_method":41,"function_call_generic":42,"method_call_generic":43,"function_call_header_with_parameters":44,"function_call_header_no_parameters":45,"function_call_header":46,"VOID":47,"assignment_expression":48,",":49,"type_specifier":50,"FIELD_SELECTION":51,"method_call_header_with_parameters":52,"method_call_header_no_parameters":53,"method_call_header":54,"unary_expression":55,"unary_operator":56,"+":57,"-":58,"!":59,"~":60,"multiplicative_expression":61,"*":62,"/":63,"%":64,"additive_expression":65,"shift_expression":66,"<<":67,">>":68,"relational_expression":69,"<":70,">":71,"<=":72,">=":73,"equality_expression":74,"==":75,"!=":76,"and_expression":77,"&":78,"exclusive_or_expression":79,"^":80,"inclusive_or_expression":81,"|":82,"logical_and_expression":83,"&&":84,"logical_xor_expression":85,"^^":86,"logical_or_expression":87,"||":88,"conditional_expression":89,"?":90,"assignment_operator":91,"=":92,"*=":93,"/=":94,"%=":95,"+=":96,"-=":97,"<<=":98,">>=":99,"&=":100,"^=":101,"|=":102,"constant_expression":103,"declaration":104,"function_prototype":105,";":106,"init_declarator_list":107,"PRECISION":108,"precision_qualifier":109,"type_specifier_no_prec":110,"function_declarator":111,"function_header":112,"function_header_with_parameters":113,"parameter_declaration":114,"fully_specified_type":115,"parameter_declarator":116,"parameter_type_qualifier":117,"parameter_qualifier":118,"parameter_type_specifier":119,"IN":120,"OUT":121,"INOUT":122,"single_declaration":123,"initializer":124,"INVARIANT":125,"type_qualifier":126,"layout_qualifier":127,"LAYOUT":128,"layout_qualifier_id_list":129,"layout_qualifier_id":130,"interpolation_qualifier":131,"SMOOTH":132,"FLAT":133,"NOPERSPECTIVE":134,"CONST":135,"storage_qualifier":136,"ATTRIBUTE":137,"VARYING":138,"CENTROID":139,"UNIFORM":140,"type_specifier_nonarray":141,"basic_type_specifier_nonarray":142,"struct_specifier":143,"FLOAT":144,"DOUBLE":145,"INT":146,"BOOL":147,"VEC2":148,"VEC3":149,"VEC4":150,"BVEC2":151,"BVEC3":152,"BVEC4":153,"IVEC2":154,"IVEC3":155,"IVEC4":156,"MAT2X2":157,"MAT3X3":158,"MAT4X4":159,"SAMPLER1D":160,"SAMPLER2D":161,"SAMPLER3D":162,"SAMPLERCUBE":163,"SAMPLER1DSHADOW":164,"SAMPLER2DSHADOW":165,"HIGHP":166,"MEDIUMP":167,"LOWP":168,"STRUCT":169,"{":170,"struct_declaration_list":171,"}":172,"struct_declaration":173,"struct_declarator_list":174,"struct_declarator":175,"declaration_statement":176,"statement":177,"compound_statement":178,"simple_statement":179,"expression_statement":180,"selection_statement":181,"switch_statement":182,"case_label":183,"iteration_statement":184,"jump_statement":185,"statement_list":186,"statement_no_new_scope":187,"compound_statement_no_new_scope":188,"IF":189,"selection_rest_statement":190,"ELSE":191,"condition":192,"SWITCH":193,"CASE":194,"DEFAULT":195,"WHILE":196,"DO":197,"FOR":198,"for_init_statement":199,"for_rest_statement":200,"conditionopt":201,"CONTINUE":202,"BREAK":203,"RETURN":204,"DISCARD":205,"DEBUGGER":206,"function_definition":207,"$accept":0,"$end":1},
	terminals_: {2:"error",5:"EOF",9:"VERSION",10:"INTCONSTANT",11:"EOL",13:"PRAGMA_DEBUG_ON",14:"PRAGMA_DEBUG_OFF",15:"PRAGMA_OPTIMIZE_ON",16:"PRAGMA_OPTIMIZE_OFF",17:"PRAGMA_INVARIANT_ALL",21:"TYPE_IDENTIFIER",22:"EXTENSION",23:":",25:"IDENTIFIER",26:"NEW_IDENTIFIER",28:"FLOATCONSTANT",29:"BOOLCONSTANT",30:"(",32:")",34:"[",36:"]",38:".",39:"++",40:"--",47:"VOID",49:",",51:"FIELD_SELECTION",57:"+",58:"-",59:"!",60:"~",62:"*",63:"/",64:"%",67:"<<",68:">>",70:"<",71:">",72:"<=",73:">=",75:"==",76:"!=",78:"&",80:"^",82:"|",84:"&&",86:"^^",88:"||",90:"?",92:"=",93:"*=",94:"/=",95:"%=",96:"+=",97:"-=",98:"<<=",99:">>=",100:"&=",101:"^=",102:"|=",106:";",108:"PRECISION",120:"IN",121:"OUT",122:"INOUT",125:"INVARIANT",128:"LAYOUT",132:"SMOOTH",133:"FLAT",134:"NOPERSPECTIVE",135:"CONST",137:"ATTRIBUTE",138:"VARYING",139:"CENTROID",140:"UNIFORM",144:"FLOAT",145:"DOUBLE",146:"INT",147:"BOOL",148:"VEC2",149:"VEC3",150:"VEC4",151:"BVEC2",152:"BVEC3",153:"BVEC4",154:"IVEC2",155:"IVEC3",156:"IVEC4",157:"MAT2X2",158:"MAT3X3",159:"MAT4X4",160:"SAMPLER1D",161:"SAMPLER2D",162:"SAMPLER3D",163:"SAMPLERCUBE",164:"SAMPLER1DSHADOW",165:"SAMPLER2DSHADOW",166:"HIGHP",167:"MEDIUMP",168:"LOWP",169:"STRUCT",170:"{",172:"}",189:"IF",191:"ELSE",193:"SWITCH",194:"CASE",195:"DEFAULT",196:"WHILE",197:"DO",198:"FOR",202:"CONTINUE",203:"BREAK",204:"RETURN",205:"DISCARD",206:"DEBUGGER"},
	productions_: [0,[3,2],[4,3],[6,0],[6,3],[12,2],[12,2],[12,2],[12,2],[12,2],[7,0],[7,2],[19,1],[19,1],[18,5],[8,1],[8,2],[20,1],[20,1],[27,1],[27,1],[27,1],[27,1],[27,3],[33,1],[33,4],[33,1],[33,3],[33,2],[33,2],[35,1],[37,1],[41,1],[41,3],[42,2],[42,2],[45,2],[45,1],[44,2],[44,3],[46,2],[46,2],[46,1],[43,2],[43,2],[53,2],[53,1],[52,2],[52,3],[54,2],[55,1],[55,2],[55,2],[55,2],[56,1],[56,1],[56,1],[56,1],[61,1],[61,3],[61,3],[61,3],[65,1],[65,3],[65,3],[66,1],[66,3],[66,3],[69,1],[69,3],[69,3],[69,3],[69,3],[74,1],[74,3],[74,3],[77,1],[77,3],[79,1],[79,3],[81,1],[81,3],[83,1],[83,3],[85,1],[85,3],[87,1],[87,3],[89,1],[89,5],[48,1],[48,3],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[31,1],[31,3],[103,1],[104,2],[104,2],[104,4],[105,2],[111,1],[111,1],[113,2],[113,3],[112,3],[116,2],[116,5],[114,3],[114,2],[114,3],[114,2],[118,0],[118,1],[118,1],[118,1],[119,1],[107,1],[107,3],[107,5],[107,6],[107,7],[107,8],[107,5],[123,1],[123,2],[123,4],[123,5],[123,6],[123,7],[123,4],[123,2],[115,1],[115,2],[127,4],[129,1],[129,3],[130,1],[130,3],[131,1],[131,1],[131,1],[117,1],[126,1],[126,1],[126,2],[126,1],[126,2],[126,2],[126,3],[126,1],[136,1],[136,1],[136,1],[136,2],[136,1],[136,1],[136,2],[136,2],[136,1],[50,1],[50,2],[110,1],[110,3],[110,4],[141,1],[141,1],[141,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[109,1],[109,1],[109,1],[143,5],[143,4],[171,1],[171,2],[173,3],[174,1],[174,3],[175,1],[175,4],[124,1],[176,1],[177,1],[177,1],[179,1],[179,1],[179,1],[179,1],[179,1],[179,1],[179,1],[178,2],[178,3],[187,1],[187,1],[188,2],[188,3],[186,1],[186,2],[180,1],[180,2],[181,5],[190,3],[190,1],[192,1],[192,4],[182,5],[183,3],[183,2],[184,5],[184,7],[184,6],[199,1],[199,1],[201,1],[201,0],[200,2],[200,3],[185,2],[185,2],[185,2],[185,3],[185,2],[185,2],[24,1],[24,1],[24,1],[207,2]],
	performAction: function anonymous(yytext, yyleng, yylineno, yy, yystate , $$ , _$ ) {

		var $0 = $$.length - 1;
	switch (yystate) {
	case 1:
	 return $$[$0-1]; 
	break;
	case 15: case 16:

					if ($$[$0] !== null) {
					yy.state.addAstNode($$[$0]);
				}

				break;
	case 19:

						this.$ = new AstExpression('ident');
					this.$.setLocation(_$[$0]);
					this.$.primary_expression.identifier = $$[$0]; 
	break;
	case 20:

						this.$ = new AstExpression('int');
					this.$.setLocation(_$[$0]);
					this.$.primary_expression.int_constant = $$[$0];
					this.$.primary_expression.type = 'int'; 
	break;
	case 21:

						this.$ = new AstExpression('float');
					this.$.setLocation(_$[$0]);
					this.$.primary_expression.float_constant = $$[$0];
					this.$.primary_expression.type = 'float'; 
	break;
	case 22:

						this.$ = new AstExpression('bool');
					this.$.setLocation(_$[$0]);
					this.$.primary_expression.bool_constant = $$[$0];
					this.$.primary_expression.type = 'bool'; 
	break;
	case 23:

						this.$ = $$[$0-1];
					this.$.grouped = true;

				break;
	case 25:

						this.$ = new AstExpression('[]', $$[$0-3], $$[$0-1]);
					this.$.setLocation(_$[$0-3]); 
	break;
	case 27:

						this.$ = new AstExpression('.', $$[$0-2]);
					this.$.setLocation(_$[$0-2]);
					this.$.primary_expression.identifier = $$[$0]; 
	break;
	case 28:

						this.$ = new AstExpression('x++', $$[$0-1]);
					this.$.setLocation(_$[$0-1]); 
	break;
	case 29:

						this.$ = new AstExpression('x--', $$[$0-1]);
					this.$.setLocation(_$[$0-1]); 
	break;
	case 38:

						this.$ = $$[$0-1];
					this.$.setLocation(_$[$0-1]);
					this.$.expressions.push($$[$0]); 
	break;
	case 39:

						this.$ = $$[$0-2];
					this.$.setLocation(_$[$0-2]);
					this.$.expressions.push($$[$0]); 
	break;
	case 40:

						this.$ = new AstFunctionExpression($$[$0-1]);
					this.$.setLocation(_$[$0-1]);

					break;
	case 41:

						var callee = new AstExpression($$[$0-1]);
					this.$ = new AstFunctionExpression(callee);
					this.$.setLocation(_$[$0-1]); 
	break;
	case 51:

						this.$ = new AstExpression('++x', $$[$0]);
					this.$.setLocation(_$[$0-1]);

					break;
	case 52:

						this.$ = new AstExpression('--x', $$[$0]);
					this.$.setLocation(_$[$0-1]);

					break;
	case 53:

						this.$ = new AstExpression($$[$0-1], $$[$0]);
					this.$.setLocation(_$[$0-1]);

					break;
	case 54:

						this.$ = 'POS'; 
	break;
	case 55:

						this.$ = 'NEG'; 
	break;
	case 59: case 60: case 61: case 63: case 64: case 66: case 67: case 69: case 70: case 71: case 72: case 74: case 75: case 77: case 79: case 81: case 83: case 85: case 87:

						this.$ = new AstExpressionBin($$[$0-1], $$[$0-2], $$[$0]);
					this.$.setLocation(_$[$0-2]);

					break;
	case 89:

						this.$ = new AstExpression($$[$0-3], $$[$0-4], $$[$0-2], $$[$0]);
					this.$.setLocation(_$[$0-4]);

					break;
	case 91:

						this.$ = new AstExpression($$[$0-1], $$[$0-2], $$[$0]);
					this.$.setLocation(_$[$0-2]);

					break;
	case 103:

						this.$ = $$[$0];

					break;
	case 104:

						if ($$[$0-2].oper !== $$[$0-1]) {
						this.$ = new AstExpression($$[$0-1]);
						this.$.setLocation(_$[$0-2]);
						this.$.expressions.push($$[$0-2]);
					} else {
						this.$ = $$[$0-2];
					}
					this.$.expressions.push($$[$0]);

					break;
	case 106:

						yy.state.symbols.pop_scope();
					this.$ = $$[$0-1];

					break;
	case 107:

						this.$ = $$[$0-1];

					break;
	case 108:

						$$[$0-1].precision = $$[$0-2];
					$$[$0-1].is_precision_statement = true;
					this.$ = $$[$0-1]; 
	break;
	case 112:

					  	this.$ = $$[$0-1];
					this.$.parameters.push($$[$0]);

					break;
	case 113:

					  	this.$ = $$[$0-2];
					this.$.parameters.push($$[$0]);

	        	break;
	case 114:

						this.$ = new AstFunction();
					this.$.setLocation(_$[$0-2]);
					this.$.return_type = $$[$0-2];
					this.$.identifier = $$[$0-1];

					if ($$[$0-1] == 'main') {
						if (yy.state.symbols.get_function($$[$0-1])) {
							var e = new Error("Cannot define main() more than once");
							e.lineNumber = _$[$0-2].first_line;
							e.columnNumber = _$[$0-2].first_column;
							throw e;
						}
					}

						this.$.entry = yy.state.symbols.add_function($$[$0-1], $$[$0-2].specifier.type_name);
					this.$.entry.Ast = this.$;
					yy.state.symbols.push_scope();

					break;
	case 115:

						this.$ = new AstParameterDeclarator();
					this.$.setLocation(_$[$0-1]);
					this.$.type = new AstFullySpecifiedType();
					this.$.type.setLocation(_$[$0-1]);
					this.$.type.specifier = $$[$0-1];
					this.$.identifier = $$[$0]; 
	break;
	case 117:

						$$[$0-2].concat($$[$0-1]);
					this.$ = $$[$0];
					this.$.type.qualifier = $$[$0-2]; 
	break;
	case 118:

						this.$ = $$[$0];
					this.$.type.qualifier = $$[$0-1]; 
	break;
	case 119:

						$$[$0-2].concat($$[$0-1]);
					this.$ = new AstParameterDeclarator();
					this.$.setLocation(_$[$0-2]);
					this.$.type = new AstFullySpecifiedType();
					this.$.type.qualifier = $$[$0-2];
					this.$.type.specifier = $$[$0]; 
	break;
	case 120:

						this.$ = new AstParameterDeclarator();
					this.$.setLocation(_$[$0-1]);
					this.$.type = new AstFullySpecifiedType();
					this.$.type.qualifier = $$[$0-1];
					this.$.type.specifier = $$[$0]; 
	break;
	case 121:

					  this.$ = []; 
	break;
	case 122:

					this.$ = ['in']; 
	break;
	case 123:

					this.$ = ['out']; 
	break;
	case 124:

					this.$ = ['inout']; 
	break;
	case 127:

					var decl = new AstDeclaration($$[$0], false);
				decl.setLocation(_$[$0-2]);
				this.$ = $$[$0-2];
				this.$.declarations.push(decl);
	break;
	case 129:

					var decl = new AstDeclaration($$[$0-3], true, $$[$0-1]);
				decl.setLocation(_$[$0-5]);
				this.$ = $$[$0-5];
				this.$.declarations.push(decl);
	break;
	case 132:

					var decl = new AstDeclaration($$[$0-2], false, null, $$[$0]);
				decl.setLocation(_$[$0-4]);
				this.$ = $$[$0-4];
				this.$.declarations.push(decl);
	break;
	case 133:

						if ($$[$0].specifier.type_specifier !== types.struct) {
						yy.state.addError("empty declaration list", _$[$0].first_line, _$[$0].first_column);
						return 0;
					}

						this.$ = new AstDeclaratorList($$[$0]);
					this.$.setLocation(_$[$0]); 
	break;
	case 134:

						var decl = new AstDeclaration($$[$0], false);
					decl.setLocation(_$[$0]);
					this.$ = new AstDeclaratorList($$[$0-1]);
					this.$.setLocation(_$[$0-1]);
					this.$.declarations.push(decl); 
	break;
	case 135:

						var decl = new AstDeclaration($$[$0-2], true);
					decl.setLocation(_$[$0-2]);
					this.$ = new AstDeclaratorList($$[$0-3]);
					this.$.setLocation(_$[$0-3]);
					this.$.declarations.push(decl); 
	break;
	case 136:

						var decl = new AstDeclaration($$[$0-3], true, $$[$0-1]);
					decl.setLocation(_$[$0-3]);
					this.$ = new AstDeclaratorList($$[$0-4]);
					this.$.setLocation(_$[$0-4]);
					this.$.declarations.push(decl); 
	break;
	case 137:

						var decl = new AstDeclaration($$[$0-4], true, null, $$[$0]);
					decl.setLocation(_$[$0-4]);
					this.$ = new AstDeclaratorList($$[$0-5]);
					this.$.setLocation(_$[$0-5]);
					this.$.declarations.push(decl); 
	break;
	case 138:

						var decl = new AstDeclaration($$[$0-5], true, $$[$0-3], $$[$0]);
					decl.setLocation(_$[$0-5]);
					this.$ = new AstDeclaratorList($$[$0-6]);
					this.$.setLocation(_$[$0-6]);
					this.$.declarations.push(decl); 
	break;
	case 139:

						var decl = new AstDeclaration($$[$0-2], false, null, $$[$0]);
					decl.setLocation(_$[$0-2]);
					this.$ = new AstDeclaratorList($$[$0-3]);
					this.$.setLocation(_$[$0-3]);
					this.$.declarations.push(decl); 
	break;
	case 141:

						this.$ = new AstFullySpecifiedType();
					this.$.setLocation(_$[$0]);
					this.$.specifier = $$[$0]; 
	break;
	case 142:

						this.$ = new AstFullySpecifiedType();
					this.$.setLocation(_$[$0-1]);
					this.$.qualifier = $$[$0-1];
					this.$.specifier = $$[$0]; 
	break;
	case 143:

						this.$ = $$[$0-1]; 
	break;
	case 151: case 160:

						this.$ = ['const']; 
	break;
	case 161:

						this.$ = ['attribute']; 
	break;
	case 162:

						this.$ = ['varying']; 
	break;
	case 163:

						this.$ = ['centroid', 'varying']; 
	break;
	case 164:

						this.$ = ['in']; 
	break;
	case 165:

						this.$ = ['out']; 
	break;
	case 166:

						this.$ = ['centroid', 'in']; 
	break;
	case 167:

						this.$ = ['centroid', 'out']; 
	break;
	case 168:

						this.$ = ['uniform']; 
	break;
	case 169:

						this.$ = $$[$0];  

					break;
	case 170:

						this.$ = $$[$0];
					this.$.precision = $$[$0-1];

					break;
	case 174: case 175: case 176:

				  	this.$ = new AstTypeSpecifier($$[$0]);
				this.$.setLocation(_$[$0]);

				break;
	case 200:

						this.$ = ast_precision.highp; 
	break;
	case 201:

						this.$ = ast_precision.mediump; 
	break;
	case 202:

						this.$ = ast_precision.lowp; 
	break;
	case 203:

						this.$ = new AstStructSpecifier($$[$0-3], $$[$0-1]);
					this.$.setLocation(_$[$0-4]);
					yy.state.symbols.add_type($$[$0-3], types._void);

					break;
	case 205:

						this.$ = [$$[$0]];

					break;
	case 206:

						this.$ = $$[$0-1];
					this.$.push($$[$0]);

					break;
	case 207:

						var type = new AstFullySpecifiedType();
					type.setLocation(_$[$0-2]);
					type.specifier = $$[$0-2];

										this.$ = new AstDeclaratorList(type);
					this.$.setLocation(_$[$0-2]);
					this.$.declarations = $$[$0-1]; 
	break;
	case 210:

						this.$ = new AstDeclaration($$[$0], false);
					this.$.setLocation(_$[$0]);
					yy.state.symbols.add_variable($$[$0]);

					break;
	case 219: case 220: case 258:

						this.$ = null; 
	break;
	case 223:

						this.$ = new AstCompoundStatement(true);
					this.$.setLocation(_$[$0-1]); 
	break;
	case 224:

					  	yy.state.symbols.push_scope();
					this.$ = new AstCompoundStatement(true, $$[$0-1]);
					this.$.setLocation(_$[$0-2]);
					yy.state.symbols.pop_scope(); 
	break;
	case 228:

						this.$ = new AstCompoundStatement(false, $$[$0-1]);
					this.$.setLocation(_$[$0-2]); 
	break;
	case 229:

						if ($$[$0] === null) {
						yy.state.addError("<nil> statement", _$[$0].first_line, _$[$0].first_column);
					} else {
						this.$ = [$$[$0]];
					}

					break;
	case 230:

						if ($$[$0] === null) {
						yy.state.addError("<nil> statement", _$[$0-1].first_line, _$[$0-1].first_column);
					}
					this.$ = $$[$0-1];
					this.$.push($$[$0]);

					break;
	case 232:

						this.$ = new AstExpressionStatement($$[$0-1]);
					this.$.setLocation(_$[$0-1]); 
	break;
	case 233:

						this.$ = new AstSelectionStatement($$[$0-2], $$[$0].then_statement, $$[$0].else_statement);
					this.$.setLocation(_$[$0-4]); 
	break;
	case 234:

				  		this.$ = {};
					this.$.then_statement = $$[$0-2];
					this.$.else_statement = $$[$0]; 
	break;
	case 235:

						this.$.then_statement = $$[$0]; 
	break;
	case 250:

					this.$ = new AstJumpStatement('continue');
				this.$.setLocation(_$[$0-1]); 
	break;
	case 251:

					this.$ = new AstJumpStatement('break');
				this.$.setLocation(_$[$0-1]); 
	break;
	case 252:

					this.$ = new AstJumpStatement('return');
				this.$.setLocation(_$[$0-1]); 
	break;
	case 253:

					this.$ = new AstJumpStatement('return', $$[$0-1]);
				this.$.setLocation(_$[$0-2]); 
	break;
	case 254:
				this.$ = new AstJumpStatement('discard');
				this.$.setLocation(_$[$0-1]); 
	break;
	case 255:

					this.$ = new AstJumpStatement('debugger');
				this.$.setLocation(_$[$0-1]); 
	break;
	case 256: case 257:

						this.$ = $$[$0]; 
	break;
	case 259:

						this.$ = new AstFunctionDefinition();
					this.$.setLocation(_$[$0-1]);
					this.$.proto_type = $$[$0-1];
					this.$.body = $$[$0];
					yy.state.symbols.pop_scope(); 
	break;
	}
	},
	table: [o($V0,[2,3],{3:1,4:2,6:3,9:[1,4]}),{1:[3]},{5:[1,5]},o($V0,[2,10],{7:6}),{10:[1,7]},{1:[2,1]},{8:8,12:14,13:$V1,14:$V2,15:$V3,16:$V4,17:$V5,18:9,21:$V6,22:[1,11],24:10,47:$V7,50:29,104:13,105:15,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,207:12},{11:[1,78]},{5:[2,2],12:14,13:$V1,14:$V2,15:$V3,16:$V4,17:$V5,21:$V6,24:79,47:$V7,50:29,104:13,105:15,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,207:12},o($V0,[2,11]),o($VL,[2,15]),{19:80,20:81,21:$VM,25:$VN,26:$VO},o($VL,[2,256]),o($VL,[2,257]),o($VL,[2,258]),{106:$VP,170:$VQ,188:85},{49:[1,89],106:[1,88]},{109:90,166:$VH,167:$VI,168:$VJ},{11:[1,91]},{11:[1,92]},{11:[1,93]},{11:[1,94]},{11:[1,95]},{32:[1,96]},o($VR,[2,126]),o($VS,$VT,{114:97,117:98,118:99,32:[2,110],120:$VU,121:$VV,122:$VW,135:$VX}),{32:[2,111],49:[1,104]},o($VR,[2,133],{19:105,20:106,21:$VM,25:$VN,26:$VO}),o($VS,$VY,{20:107,136:108,131:109,25:$VN,26:$VO,120:$V9,121:$Va,132:$Vd,133:$Ve,134:$Vf,135:$Vg,137:$Vh,138:$Vi,139:$Vj,140:$Vk}),o($VZ,$V_),{21:$V6,47:$V7,50:110,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($V$,[2,169]),{21:$V6,47:$V7,110:111,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,169:$VK},o($VS,[2,152]),o($VS,[2,153],{136:112,120:$V9,121:$Va,135:$Vg,137:$Vh,138:$Vi,139:$Vj,140:$Vk}),o($VS,[2,155],{136:113,120:$V9,121:$Va,135:$Vg,137:$Vh,138:$Vi,139:$Vj,140:$Vk}),o($V$,[2,171],{34:[1,114]}),o($V01,[2,200]),o($V01,[2,201]),o($V01,[2,202]),o($VS,[2,160]),o($VS,[2,161]),o($VS,[2,162]),{120:[1,116],121:[1,117],138:[1,115]},o($VS,[2,164]),o($VS,[2,165]),o($VS,[2,168]),{30:[1,118]},o($V11,[2,148]),o($V11,[2,149]),o($V11,[2,150]),o($V21,[2,174]),o($V21,[2,175]),o($V21,[2,176]),o($V21,$V31),o($V21,[2,178]),o($V21,[2,179]),o($V21,[2,180]),o($V21,[2,181]),o($V21,[2,182]),o($V21,[2,183]),o($V21,[2,184]),o($V21,[2,185]),o($V21,[2,186]),o($V21,[2,187]),o($V21,[2,188]),o($V21,[2,189]),o($V21,[2,190]),o($V21,[2,191]),o($V21,[2,192]),o($V21,[2,193]),o($V21,[2,194]),o($V21,[2,195]),o($V21,[2,196]),o($V21,[2,197]),o($V21,[2,198]),o($V21,[2,199]),{19:119,20:81,21:$VM,25:$VN,26:$VO,170:[1,120]},o($V0,[2,4]),o($VL,[2,16]),{23:[1,121]},o([11,23,32,34,49,92,106,170],$V41),o([11,23,32,34,36,38,39,40,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,92,93,94,95,96,97,98,99,100,101,102,106,170],[2,13]),o($V51,[2,17]),o($V51,[2,18]),o($VL,[2,259]),o($V61,[2,106]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,172:[1,122],176:128,177:124,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,186:123,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},o($V61,[2,107]),{19:187,20:81,21:$VM,25:$VN,26:$VO},{21:$V6,47:$V7,110:188,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,169:$VK},o($VL,[2,5]),o($VL,[2,6]),o($VL,[2,7]),o($VL,[2,8]),o($VL,[2,9]),o([106,170],[2,109]),o($Vw1,[2,112]),o($VS,$VT,{118:189,120:$VU,121:$VV,122:$VW}),{21:$V6,47:$V7,50:192,109:32,110:31,116:190,119:191,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o([21,47,120,121,122,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],[2,151]),o($VS,[2,122]),o($VS,[2,123]),o($VS,[2,124]),o($VS,$VT,{117:98,118:99,114:193,120:$VU,121:$VV,122:$VW,135:$VX}),o($VR,[2,134],{34:[1,194],92:[1,195]}),o([34,49,92,106],$V41,{30:[1,196]}),o($VR,[2,140]),o($VS,[2,157]),{120:$V9,121:$Va,135:$Vg,136:197,137:$Vh,138:$Vi,139:$Vj,140:$Vk},o($VZ,[2,142]),o($V$,[2,170]),o($VS,[2,154]),o($VS,[2,156]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,36:[1,198],37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:200,103:199,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VS,[2,163]),o($VS,[2,166]),o($VS,[2,167]),{19:205,20:81,21:$VM,25:$VN,26:$VO,129:203,130:204},{170:[1,206]},{21:$V6,47:$V7,50:209,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,171:207,173:208},{19:210,20:81,21:$VM,25:$VN,26:$VO},o($V61,[2,227]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,172:[1,211],176:128,177:212,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},o($Vx1,[2,229]),o($Vy1,[2,214]),o($Vy1,[2,215]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,172:[1,213],176:128,177:124,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,186:214,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},o($Vy1,[2,216]),o($Vy1,[2,217]),o($Vy1,[2,218]),o($Vy1,[2,219]),o($Vy1,[2,220]),o($Vy1,[2,221]),o($Vy1,[2,222]),o($Vy1,[2,213]),o($Vy1,[2,231]),{49:$Vz1,106:[1,215]},{30:[1,217]},{30:[1,218]},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:219,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{23:[1,220]},{30:[1,221]},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,176:128,177:222,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},{30:[1,223]},{106:[1,224]},{106:[1,225]},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:227,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,106:[1,226],109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{106:[1,228]},{106:[1,229]},{106:$VP},o($VA1,[2,103]),o($VA1,[2,90]),o($VB1,$VC1,{91:230,92:[1,231],93:[1,232],94:[1,233],95:[1,234],96:[1,235],97:[1,236],98:[1,237],99:[1,238],100:[1,239],101:[1,240],102:[1,241]}),o($VA1,[2,88],{88:[1,243],90:[1,242]}),o($VD1,[2,50],{34:[1,244],38:[1,245],39:[1,246],40:[1,247]}),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:248,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:249,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:250,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VZ,$V_,{30:$VE1}),o($VF1,[2,86],{86:$VG1}),o($VH1,[2,24]),o($VH1,[2,26]),o($VI1,[2,54]),o($VI1,[2,55]),o($VI1,[2,56]),o($VI1,[2,57]),o($VJ1,[2,84],{84:$VK1}),o($VH1,[2,19],{30:[1,254]}),o($VH1,[2,20]),o($VH1,[2,21]),o($VH1,[2,22]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:255,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VH1,[2,31]),o($VL1,[2,82],{82:$VM1}),o($VH1,[2,32]),o($VN1,[2,80],{80:$VO1}),{32:[1,258],49:[1,259]},{32:[1,260]},o($VP1,[2,78],{78:$VQ1}),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,32:[2,37],33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:[1,263],48:262,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VR1,[2,76],{75:$VS1,76:$VT1}),o($VU1,[2,42]),o($VV1,[2,73],{70:$VW1,71:$VX1,72:$VY1,73:$VZ1}),o($V_1,[2,68],{67:$V$1,68:$V02}),o($V12,[2,65],{57:$V22,58:$V32}),o($V42,[2,62],{62:$V52,63:$V62,64:$V72}),o($VR,[2,127],{34:[1,277],92:[1,278]}),{106:[1,279]},{21:$V6,47:$V7,50:192,109:32,110:31,116:280,119:281,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($Vw1,[2,118]),o($Vw1,[2,120]),o($Vw1,[2,125],{20:81,19:282,21:$VM,25:$VN,26:$VO}),o($Vw1,[2,113]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,36:[1,283],37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:200,103:284,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:286,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,124:285,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o([21,32,47,120,121,122,135,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],[2,114]),o($VS,[2,158]),o($V$,[2,172]),{36:[1,287]},{36:[2,105]},o($VB1,$VC1),{30:$VE1},{32:[1,288],49:[1,289]},o($Vw1,[2,144]),o($Vw1,[2,146],{92:[1,290]}),{21:$V6,47:$V7,50:209,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,171:291,173:208},{21:$V6,47:$V7,50:209,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,172:[1,292],173:293},o($V82,[2,205]),{19:296,20:81,21:$VM,25:$VN,26:$VO,174:294,175:295},{11:[1,297]},o($V61,[2,228]),o($Vx1,[2,230]),o($Vy1,[2,223]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,172:[1,298],176:128,177:212,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},o($Vy1,[2,232]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:299,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:300,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:301,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{23:[1,302],49:$Vz1},o($Vy1,[2,240]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:304,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,115:305,120:$V9,121:$Va,125:$V92,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,192:303},{196:[1,307]},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,176:310,180:309,199:308},o($Vy1,[2,250]),o($Vy1,[2,251]),o($Vy1,[2,252]),{49:$Vz1,106:[1,311]},o($Vy1,[2,254]),o($Vy1,[2,255]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:312,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VI1,[2,92]),o($VI1,[2,93]),o($VI1,[2,94]),o($VI1,[2,95]),o($VI1,[2,96]),o($VI1,[2,97]),o($VI1,[2,98]),o($VI1,[2,99]),o($VI1,[2,100]),o($VI1,[2,101]),o($VI1,[2,102]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:313,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:314,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:316,33:155,35:315,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{19:317,20:319,21:$VM,25:$VN,26:$VO,43:318,52:320,53:321,54:322},o($VH1,[2,28]),o($VH1,[2,29]),o($VD1,[2,51]),o($VD1,[2,52]),o($VD1,[2,53]),o($VU1,[2,40]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:323,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:324,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VU1,[2,41]),{32:[1,325],49:$Vz1},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:326,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:327,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VH1,[2,34]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:328,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VH1,[2,35]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:329,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($Vw1,[2,38]),o($Va2,$V31,{32:[2,36]}),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:330,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:331,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:332,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:333,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:334,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:335,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:336,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:337,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:338,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:339,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:340,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:341,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:342,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,36:[1,343],37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:200,103:344,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:286,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,124:345,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($V61,[2,108]),o($Vw1,[2,117]),o($Vw1,[2,119]),o($Vw1,[2,115],{34:[1,346]}),o($VR,[2,135],{92:[1,347]}),{36:[1,348]},o($VR,[2,139]),o([32,49,106],[2,212]),o($V$,[2,173]),o($V11,[2,143]),{19:205,20:81,21:$VM,25:$VN,26:$VO,130:349},{10:[1,350]},{21:$V6,47:$V7,50:209,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,172:[1,351],173:293},o($V21,[2,204]),o($V82,[2,206]),{49:[1,353],106:[1,352]},o($VR,[2,208]),o($VR,[2,210],{34:[1,354]}),o($V0,[2,14]),o($Vy1,[2,224]),o($VA1,[2,104]),{32:[1,355],49:$Vz1},{32:[1,356],49:$Vz1},o($Vy1,[2,239]),{32:[1,357]},o($Vb2,[2,236],{49:$Vz1}),{19:358,20:81,21:$VM,25:$VN,26:$VO},o($VS,$VY,{136:108,131:109,120:$V9,121:$Va,132:$Vd,133:$Ve,134:$Vf,135:$Vg,137:$Vh,138:$Vi,139:$Vj,140:$Vk}),{30:[1,359]},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:304,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,106:[2,247],109:32,110:31,115:305,120:$V9,121:$Va,125:$V92,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,192:362,200:360,201:361},o($Vc2,[2,244]),o($Vc2,[2,245]),o($Vy1,[2,253]),o($VA1,[2,91]),{23:[1,363],49:$Vz1},o($VF1,[2,87],{86:$VG1}),{36:[1,364]},{36:[2,30],49:$Vz1},o($VH1,[2,27]),o($VH1,[2,33]),o($VH1,$V41,{30:[1,365]}),{32:[1,366],49:[1,367]},{32:[1,368]},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,32:[2,46],33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:[1,370],48:369,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VJ1,[2,85],{84:$VK1}),o($VL1,[2,83],{82:$VM1}),o($VH1,[2,23]),o($VN1,[2,81],{80:$VO1}),o($VP1,[2,79],{78:$VQ1}),o($Vw1,[2,39]),o($VR1,[2,77],{75:$VS1,76:$VT1}),o($VV1,[2,74],{70:$VW1,71:$VX1,72:$VY1,73:$VZ1}),o($VV1,[2,75],{70:$VW1,71:$VX1,72:$VY1,73:$VZ1}),o($V_1,[2,69],{67:$V$1,68:$V02}),o($V_1,[2,70],{67:$V$1,68:$V02}),o($V_1,[2,71],{67:$V$1,68:$V02}),o($V_1,[2,72],{67:$V$1,68:$V02}),o($V12,[2,66],{57:$V22,58:$V32}),o($V12,[2,67],{57:$V22,58:$V32}),o($V42,[2,63],{62:$V52,63:$V62,64:$V72}),o($V42,[2,64],{62:$V52,63:$V62,64:$V72}),o($VB1,[2,59]),o($VB1,[2,60]),o($VB1,[2,61]),o($VR,[2,128],{92:[1,371]}),{36:[1,372]},o($VR,[2,132]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:200,103:373,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:286,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,124:374,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VR,[2,136],{92:[1,375]}),o($Vw1,[2,145]),o($Vw1,[2,147]),o($V21,[2,203]),o($V82,[2,207]),{19:296,20:81,21:$VM,25:$VN,26:$VO,175:376},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,50:202,51:$Vd1,55:201,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:200,103:377,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,176:128,177:379,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,190:378,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},{170:$Vj1,178:380},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$VQ,176:128,179:383,180:129,181:130,182:131,183:132,184:133,185:134,187:381,188:382,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},{92:[1,384]},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:385,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{32:[1,386]},{106:[1,387]},{106:[2,246]},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:388,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VH1,[2,25]),o($VU1,[2,49]),o($VH1,[2,43]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:389,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VH1,[2,44]),o($Vw1,[2,47]),o($Va2,$V31,{32:[2,45]}),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:286,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,124:390,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VR,[2,129],{92:[1,391]}),{36:[1,392]},o($VR,[2,137]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:286,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,124:393,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VR,[2,209]),{36:[1,394]},o($Vy1,[2,233]),o($Vx1,[2,235],{191:[1,395]}),o($Vy1,[2,238]),o($Vy1,[2,241]),o($Vy1,[2,225]),o($Vy1,[2,226]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:286,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,124:396,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{32:[1,397],49:$Vz1},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$VQ,176:128,179:383,180:129,181:130,182:131,183:132,184:133,185:134,187:398,188:382,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:399,32:[2,248],33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VA1,[2,89]),o($Vw1,[2,48]),o($VR,[2,130]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:286,50:202,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,109:32,110:31,124:400,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($Vw1,[2,116]),o($VR,[2,138]),o($VR,[2,211]),{10:$V71,20:168,21:$V6,25:$VN,26:$VO,27:161,28:$V81,29:$V91,30:$Va1,31:137,33:155,37:162,39:$Vb1,40:$Vc1,41:173,42:175,44:177,45:178,46:180,47:$V7,48:151,50:159,51:$Vd1,55:153,56:158,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:186,65:185,66:184,69:183,74:181,77:179,79:176,81:174,83:167,85:160,87:154,89:152,104:135,105:150,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,176:128,177:401,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1,206:$Vv1},o($Vb2,[2,237]),{106:[1,402]},o($Vy1,[2,243]),{32:[2,249],49:$Vz1},o($VR,[2,131]),o($Vy1,[2,234]),o($Vy1,[2,242])],
	defaultActions: {5:[2,1],200:[2,105],362:[2,246]},
	parseError: function parseError(str, hash) {
	    if (hash.recoverable) {
	        this.trace(str);
	    } else {
	        throw new Error(str);
	    }
	},
	parse: function parse(input) {
	    var self = this, stack = [0], tstack = [], vstack = [null], lstack = [], table = this.table, yytext = '', yylineno = 0, yyleng = 0, recovering = 0, TERROR = 2, EOF = 1;
	    var args = lstack.slice.call(arguments, 1);
	    var lexer = Object.create(this.lexer);
	    var sharedState = { yy: {} };
	    for (var k in this.yy) {
	        if (Object.prototype.hasOwnProperty.call(this.yy, k)) {
	            sharedState.yy[k] = this.yy[k];
	        }
	    }
	    lexer.setInput(input, sharedState.yy);
	    sharedState.yy.lexer = lexer;
	    sharedState.yy.parser = this;
	    if (typeof lexer.yylloc == 'undefined') {
	        lexer.yylloc = {};
	    }
	    var yyloc = lexer.yylloc;
	    lstack.push(yyloc);
	    var ranges = lexer.options && lexer.options.ranges;
	    if (typeof sharedState.yy.parseError === 'function') {
	        this.parseError = sharedState.yy.parseError;
	    } else {
	        this.parseError = Object.getPrototypeOf(this).parseError;
	    }
	    function popStack(n) {
	        stack.length = stack.length - 2 * n;
	        vstack.length = vstack.length - n;
	        lstack.length = lstack.length - n;
	    }
	    _token_stack:
	        function lex() {
	            var token;
	            token = lexer.lex() || EOF;
	            if (typeof token !== 'number') {
	                token = self.symbols_[token] || token;
	            }
	            return token;
	        }
	    var symbol, preErrorSymbol, state, action, a, r, yyval = {}, p, len, newState, expected;
	    while (true) {
	        state = stack[stack.length - 1];
	        if (this.defaultActions[state]) {
	            action = this.defaultActions[state];
	        } else {
	            if (symbol === null || typeof symbol == 'undefined') {
	                symbol = lex();
	            }
	            action = table[state] && table[state][symbol];
	        }
	                    if (typeof action === 'undefined' || !action.length || !action[0]) {
	                var errStr = '';
	                expected = [];
	                for (p in table[state]) {
	                    if (this.terminals_[p] && p > TERROR) {
	                        expected.push('\'' + this.terminals_[p] + '\'');
	                    }
	                }
	                if (lexer.showPosition) {
	                    errStr = 'Parse error on line ' + (yylineno + 1) + ':\n' + lexer.showPosition() + '\nExpecting ' + expected.join(', ') + ', got \'' + (this.terminals_[symbol] || symbol) + '\'';
	                } else {
	                    errStr = 'Parse error on line ' + (yylineno + 1) + ': Unexpected ' + (symbol == EOF ? 'end of input' : '\'' + (this.terminals_[symbol] || symbol) + '\'');
	                }
	                this.parseError(errStr, {
	                    text: lexer.match,
	                    token: this.terminals_[symbol] || symbol,
	                    line: lexer.yylineno,
	                    loc: yyloc,
	                    expected: expected
	                });
	            }
	        if (action[0] instanceof Array && action.length > 1) {
	            throw new Error('Parse Error: multiple actions possible at state: ' + state + ', token: ' + symbol);
	        }
	        switch (action[0]) {
	        case 1:
	            stack.push(symbol);
	            vstack.push(lexer.yytext);
	            lstack.push(lexer.yylloc);
	            stack.push(action[1]);
	            symbol = null;
	            if (!preErrorSymbol) {
	                yyleng = lexer.yyleng;
	                yytext = lexer.yytext;
	                yylineno = lexer.yylineno;
	                yyloc = lexer.yylloc;
	                if (recovering > 0) {
	                    recovering--;
	                }
	            } else {
	                symbol = preErrorSymbol;
	                preErrorSymbol = null;
	            }
	            break;
	        case 2:
	            len = this.productions_[action[1]][1];
	            yyval.$ = vstack[vstack.length - len];
	            yyval._$ = {
	                first_line: lstack[lstack.length - (len || 1)].first_line,
	                last_line: lstack[lstack.length - 1].last_line,
	                first_column: lstack[lstack.length - (len || 1)].first_column,
	                last_column: lstack[lstack.length - 1].last_column
	            };
	            if (ranges) {
	                yyval._$.range = [
	                    lstack[lstack.length - (len || 1)].range[0],
	                    lstack[lstack.length - 1].range[1]
	                ];
	            }
	            r = this.performAction.apply(yyval, [
	                yytext,
	                yyleng,
	                yylineno,
	                sharedState.yy,
	                action[1],
	                vstack,
	                lstack
	            ].concat(args));
	            if (typeof r !== 'undefined') {
	                return r;
	            }
	            if (len) {
	                stack = stack.slice(0, -1 * len * 2);
	                vstack = vstack.slice(0, -1 * len);
	                lstack = lstack.slice(0, -1 * len);
	            }
	            stack.push(this.productions_[action[1]][0]);
	            vstack.push(yyval.$);
	            lstack.push(yyval._$);
	            newState = table[stack[stack.length - 2]][stack[stack.length - 1]];
	            stack.push(newState);
	            break;
	        case 3:
	            return true;
	        }
	    }
	    return true;
	}};

		function Parser () {
	  this.yy = {};
	}
	Parser.prototype = parser;parser.Parser = Parser;
	return new Parser;
	})();


			if (typeof require !== 'undefined' && typeof exports !== 'undefined') {
	exports.parser = parser;
	exports.Parser = parser.Parser;
	exports.parse = function () { return parser.parse.apply(parser, arguments); };
	exports.main = function commonjsMain(args) {
	    if (!args[1]) {
	        console.log('Usage: '+args[0]+' FILE');
	        process.exit(1);
	    }
	    var source = require('fs').readFileSync(require('path').normalize(args[1]), "utf8");
	    return exports.parser.parse(source);
	};
	if (typeof module !== 'undefined' && require.main === module) {
	  exports.main(process.argv.slice(1));
	}
	}
	var lexer = (function(){
	var lexer = ({

		EOF:1,

		parseError:function parseError(str, hash) {
	        if (this.yy.parser) {
	            this.yy.parser.parseError(str, hash);
	        } else {
	            throw new Error(str);
	        }
	    },

	setInput:function (input, yy) {
	        this.yy = yy || this.yy || {};
	        this._input = input;
	        this._more = this._backtrack = this.done = false;
	        this.yylineno = this.yyleng = 0;
	        this.yytext = this.matched = this.match = '';
	        this.conditionStack = ['INITIAL'];
	        this.yylloc = {
	            first_line: 1,
	            first_column: 0,
	            last_line: 1,
	            last_column: 0
	        };
	        if (this.options.ranges) {
	            this.yylloc.range = [0,0];
	        }
	        this.offset = 0;
	        return this;
	    },

	input:function () {
	        var ch = this._input[0];
	        this.yytext += ch;
	        this.yyleng++;
	        this.offset++;
	        this.match += ch;
	        this.matched += ch;
	        var lines = ch.match(/(?:\r\n?|\n).*/g);
	        if (lines) {
	            this.yylineno++;
	            this.yylloc.last_line++;
	        } else {
	            this.yylloc.last_column++;
	        }
	        if (this.options.ranges) {
	            this.yylloc.range[1]++;
	        }

		        this._input = this._input.slice(1);
	        return ch;
	    },

	unput:function (ch) {
	        var len = ch.length;
	        var lines = ch.split(/(?:\r\n?|\n)/g);

		        this._input = ch + this._input;
	        this.yytext = this.yytext.substr(0, this.yytext.length - len);
	        this.offset -= len;
	        var oldLines = this.match.split(/(?:\r\n?|\n)/g);
	        this.match = this.match.substr(0, this.match.length - 1);
	        this.matched = this.matched.substr(0, this.matched.length - 1);

		        if (lines.length - 1) {
	            this.yylineno -= lines.length - 1;
	        }
	        var r = this.yylloc.range;

		        this.yylloc = {
	            first_line: this.yylloc.first_line,
	            last_line: this.yylineno + 1,
	            first_column: this.yylloc.first_column,
	            last_column: lines ?
	                (lines.length === oldLines.length ? this.yylloc.first_column : 0)
	                 + oldLines[oldLines.length - lines.length].length - lines[0].length :
	              this.yylloc.first_column - len
	        };

		        if (this.options.ranges) {
	            this.yylloc.range = [r[0], r[0] + this.yyleng - len];
	        }
	        this.yyleng = this.yytext.length;
	        return this;
	    },

	more:function () {
	        this._more = true;
	        return this;
	    },

	reject:function () {
	        if (this.options.backtrack_lexer) {
	            this._backtrack = true;
	        } else {
	            return this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. You can only invoke reject() in the lexer when the lexer is of the backtracking persuasion (options.backtrack_lexer = true).\n' + this.showPosition(), {
	                text: "",
	                token: null,
	                line: this.yylineno
	            });

		        }
	        return this;
	    },

	less:function (n) {
	        this.unput(this.match.slice(n));
	    },

	pastInput:function () {
	        var past = this.matched.substr(0, this.matched.length - this.match.length);
	        return (past.length > 20 ? '...':'') + past.substr(-20).replace(/\n/g, "");
	    },

	upcomingInput:function () {
	        var next = this.match;
	        if (next.length < 20) {
	            next += this._input.substr(0, 20-next.length);
	        }
	        return (next.substr(0,20) + (next.length > 20 ? '...' : '')).replace(/\n/g, "");
	    },

	showPosition:function () {
	        var pre = this.pastInput();
	        var c = new Array(pre.length + 1).join("-");
	        return pre + this.upcomingInput() + "\n" + c + "^";
	    },

	test_match:function (match, indexed_rule) {
	        var token,
	            lines,
	            backup;

		        if (this.options.backtrack_lexer) {
	            backup = {
	                yylineno: this.yylineno,
	                yylloc: {
	                    first_line: this.yylloc.first_line,
	                    last_line: this.last_line,
	                    first_column: this.yylloc.first_column,
	                    last_column: this.yylloc.last_column
	                },
	                yytext: this.yytext,
	                match: this.match,
	                matches: this.matches,
	                matched: this.matched,
	                yyleng: this.yyleng,
	                offset: this.offset,
	                _more: this._more,
	                _input: this._input,
	                yy: this.yy,
	                conditionStack: this.conditionStack.slice(0),
	                done: this.done
	            };
	            if (this.options.ranges) {
	                backup.yylloc.range = this.yylloc.range.slice(0);
	            }
	        }

		        lines = match[0].match(/(?:\r\n?|\n).*/g);
	        if (lines) {
	            this.yylineno += lines.length;
	        }
	        this.yylloc = {
	            first_line: this.yylloc.last_line,
	            last_line: this.yylineno + 1,
	            first_column: this.yylloc.last_column,
	            last_column: lines ?
	                         lines[lines.length - 1].length - lines[lines.length - 1].match(/\r?\n?/)[0].length :
	                         this.yylloc.last_column + match[0].length
	        };
	        this.yytext += match[0];
	        this.match += match[0];
	        this.matches = match;
	        this.yyleng = this.yytext.length;
	        if (this.options.ranges) {
	            this.yylloc.range = [this.offset, this.offset += this.yyleng];
	        }
	        this._more = false;
	        this._backtrack = false;
	        this._input = this._input.slice(match[0].length);
	        this.matched += match[0];
	        token = this.performAction.call(this, this.yy, this, indexed_rule, this.conditionStack[this.conditionStack.length - 1]);
	        if (this.done && this._input) {
	            this.done = false;
	        }
	        if (token) {
	            return token;
	        } else if (this._backtrack) {
	            for (var k in backup) {
	                this[k] = backup[k];
	            }
	            return false; 
	        }
	        return false;
	    },

	next:function () {
	        if (this.done) {
	            return this.EOF;
	        }
	        if (!this._input) {
	            this.done = true;
	        }

		        var token,
	            match,
	            tempMatch,
	            index;
	        if (!this._more) {
	            this.yytext = '';
	            this.match = '';
	        }
	        var rules = this._currentRules();
	        for (var i = 0; i < rules.length; i++) {
	            tempMatch = this._input.match(this.rules[rules[i]]);
	            if (tempMatch && (!match || tempMatch[0].length > match[0].length)) {
	                match = tempMatch;
	                index = i;
	                if (this.options.backtrack_lexer) {
	                    token = this.test_match(tempMatch, rules[i]);
	                    if (token !== false) {
	                        return token;
	                    } else if (this._backtrack) {
	                        match = false;
	                        continue; 
	                    } else {
	                        return false;
	                    }
	                } else if (!this.options.flex) {
	                    break;
	                }
	            }
	        }
	        if (match) {
	            token = this.test_match(match, rules[index]);
	            if (token !== false) {
	                return token;
	            }
	            return false;
	        }
	        if (this._input === "") {
	            return this.EOF;
	        } else {
	            return this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. Unrecognized text.\n' + this.showPosition(), {
	                text: "",
	                token: null,
	                line: this.yylineno
	            });
	        }
	    },

	lex:function lex() {
	        var r = this.next();
	        if (r) {
	            return r;
	        } else {
	            return this.lex();
	        }
	    },

	begin:function begin(condition) {
	        this.conditionStack.push(condition);
	    },

	popState:function popState() {
	        var n = this.conditionStack.length - 1;
	        if (n > 0) {
	            return this.conditionStack.pop();
	        } else {
	            return this.conditionStack[0];
	        }
	    },

	_currentRules:function _currentRules() {
	        if (this.conditionStack.length && this.conditionStack[this.conditionStack.length - 1]) {
	            return this.conditions[this.conditionStack[this.conditionStack.length - 1]].rules;
	        } else {
	            return this.conditions["INITIAL"].rules;
	        }
	    },

	topState:function topState(n) {
	        n = this.conditionStack.length - 1 - Math.abs(n || 0);
	        if (n >= 0) {
	            return this.conditionStack[n];
	        } else {
	            return "INITIAL";
	        }
	    },

	pushState:function pushState(condition) {
	        this.begin(condition);
	    },

	stateStackSize:function stateStackSize() {
	        return this.conditionStack.length;
	    },
	options: {"moduleName":""},
	performAction: function anonymous(yy,yy_,$avoiding_name_collisions,YY_START) {

		var YYSTATE=YY_START;
	switch($avoiding_name_collisions) {
	case 0:;
	break;
	case 1:
	break;
	case 2: this.begin('PP'); return 'VERSION'; 
	break;
	case 3: this.begin('PP'); return 'EXTENSION'; 
	break;
	case 4:


				var ptr = 0;
		while (yy_.yytext.slice(0, 1) < '0' || yy_.yytext.slice(0, 1) > '9') {
			ptr++;
		}

		yy_.yylineno = parseInt(yy_.yytext.slice(0, 1), 10) - 1;
		yy_.yylloc.source = parseInt(yy_.yytext.slice(0), 10);

		break;
	case 5:
						var ptr = 0;
						while (yy_.yytext.slice(0, 1) < '0' || yy_.yytext.slice(0, 1) > '9')
							ptr++;

					   yy_.yylineno = parseInt(yy_.yytext.slice(0, 1), 10) - 1;

						break;
	case 6:
					  this.begin('PP');
					  return 'PRAGMA_DEBUG_ON';

						break;
	case 7:
					  this.begin('PP');
					  return 'PRAGMA_DEBUG_OFF';

						break;
	case 8:
					  this.begin('PP');
					  return 'PRAGMA_OPTIMIZE_ON';

						break;
	case 9:
					  this.begin('PP');
					  return 'PRAGMA_OPTIMIZE_OFF';

						break;
	case 10:
					  this.begin('PP');
					  return 'PRAGMA_INVARIANT_ALL';

						break;
	case 11: this.begin('PRAGMA'); 
	break;
	case 12: this.begin('INITIAL'); yy_.yylineno++; yycolumn = 0; 
	break;
	case 13: 
	break;
	case 14: 
	break;
	case 15: 
	break;
	case 16:return ":";
	break;
	case 17:
					   yylval.identifier = strdup(yy_.yytext);
					   return 'IDENTIFIER';

						break;
	case 18:
					    yylval.n = parseInt(yy_.yytext);
					    return 'INTCONSTANT';

						break;
	case 19: this.begin('INITIAL'); yy_.yylineno++; yycolumn = 0; return 'EOL'; 
	break;
	case 20:  
	break;
	case 21:return 'ATTRIBUTE';
	break;
	case 22:return 'CONST';
	break;
	case 23:return 'BOOL';
	break;
	case 24:return 'FLOAT';
	break;
	case 25:return 'INT';
	break;
	case 26:return 'BREAK';
	break;
	case 27:return 'CONTINUE';
	break;
	case 28:return 'DO';
	break;
	case 29:return 'WHILE';
	break;
	case 30:return 'ELSE';
	break;
	case 31:return 'FOR';
	break;
	case 32:return 'IF';
	break;
	case 33:return 'DISCARD';
	break;
	case 34:return 'RETURN';
	break;
	case 35:return 'DEBUGGER';
	break;
	case 36:return 'BVEC2';
	break;
	case 37:return 'BVEC3';
	break;
	case 38:return 'BVEC4';
	break;
	case 39:return 'IVEC2';
	break;
	case 40:return 'IVEC3';
	break;
	case 41:return 'IVEC4';
	break;
	case 42:return 'VEC2';
	break;
	case 43:return 'VEC3';
	break;
	case 44:return 'VEC4';
	break;
	case 45:return 'MAT2X2';
	break;
	case 46:return 'MAT3X3';
	break;
	case 47:return 'MAT4X4';
	break;
	case 48:return 'IN';
	break;
	case 49:return 'OUT';
	break;
	case 50:return 'INOUT';
	break;
	case 51:return 'UNIFORM';
	break;
	case 52:return 'VARYING';
	break;
	case 53:return 'INVARIANT';
	break;
	case 54:return 'FLAT';
	break;
	case 55:return 'SMOOTH';
	break;
	case 56:return 'SAMPLER1D';
	break;
	case 57:return 'SAMPLER2D';
	break;
	case 58:return 'SAMPLER3D';
	break;
	case 59:return 'SAMPLERCUBE';
	break;
	case 60:return 'SAMPLER1DSHADOW';
	break;
	case 61:return 'SAMPLER2DSHADOW';
	break;
	case 62:return 'STRUCT';
	break;
	case 63:return 'VOID';
	break;
	case 64:
	break;
	case 65:return '++';
	break;
	case 66:return '--';
	break;
	case 67:return '<=';
	break;
	case 68:return '>=';
	break;
	case 69:return '==';
	break;
	case 70:return '!=';
	break;
	case 71:return '&&';
	break;
	case 72:return '||';
	break;
	case 73:return '^^';
	break;
	case 74:return '<<';
	break;
	case 75:return '>>';
	break;
	case 76:return '*=';
	break;
	case 77:return '/=';
	break;
	case 78:return '+=';
	break;
	case 79:return '%=';
	break;
	case 80:return '<<=';
	break;
	case 81:return '>>=';
	break;
	case 82:return '&=';
	break;
	case 83:return '^=';
	break;
	case 84:return '|=';
	break;
	case 85:return '-=';
	break;
	case 86:
				    this.yylval = parseFloat(yy_.yytext);
				    return 'FLOATCONSTANT';

					break;
	case 87:
					this.yylval = parseFloat(yy_.yytext);
					return 'FLOATCONSTANT';

					break;
	case 88:
				    this.yylval = parseFloat(yy_.yytext);
				    return 'FLOATCONSTANT';

					break;
	case 89:
				    this.yylval = parseFloat(yy_.yytext);
				    return 'FLOATCONSTANT';

					break;
	case 90:
				    this.yylval = parseFloat(yy_.yytext);
				    return 'FLOATCONSTANT';

					break;
	case 91:
				    this.yylval = parseInt(yy_.yytext + 2, 16);
				    return 'INTCONSTANT';

					break;
	case 92:
				    this.yylval = parseInt(yy_.yytext, 8);
				    return 'INTCONSTANT';

					break;
	case 93:
					this.yylval = parseInt(yy_.yytext);
					return 'INTCONSTANT';

					break;
	case 94:
				    this.yylval = 1;
				    return 'BOOLCONSTANT';

					break;
	case 95:
				    this.yylval = 0;
				    return 'BOOLCONSTANT';

					break;
	case 96:return 'ASM'
	break;
	case 97:return 'CLASS'
	break;
	case 98:return 'UNION'
	break;
	case 99:return 'ENUM'
	break;
	case 100:return 'TYPEDEF'
	break;
	case 101:return 'TEMPLATE'
	break;
	case 102:return 'THIS'
	break;
	case 103:return 'PACKED'
	break;
	case 104:return 'GOTO'
	break;
	case 105:return 'SWITCH'
	break;
	case 106:return 'DEFAULT'
	break;
	case 107:return 'INLINE'
	break;
	case 108:return 'NOINLINE'
	break;
	case 109:return 'VOLATILE'
	break;
	case 110:return 'PUBLIC'
	break;
	case 111:return 'STATIC'
	break;
	case 112:return 'EXTERN'
	break;
	case 113:return 'EXTERNAL'
	break;
	case 114:return 'INTERFACE'
	break;
	case 115:return 'LONG'
	break;
	case 116:return 'SHORT'
	break;
	case 117:return 'DOUBLE'
	break;
	case 118:return 'HALF'
	break;
	case 119:return 'FIXED'
	break;
	case 120:return 'UNSIGNED'
	break;
	case 121:return 'INPUT'
	break;
	case 122:return 'OUTPUT'
	break;
	case 123:return 'HVEC2'
	break;
	case 124:return 'HVEC3'
	break;
	case 125:return 'HVEC4'
	break;
	case 126:return 'DVEC2'
	break;
	case 127:return 'DVEC3'
	break;
	case 128:return 'DVEC4'
	break;
	case 129:return 'FVEC2'
	break;
	case 130:return 'FVEC3'
	break;
	case 131:return 'FVEC4'
	break;
	case 132:return 'SAMPLER2DRECT';
	break;
	case 133:return 'SAMPLER3DRECT';
	break;
	case 134:return 'SAMPLER2DRECTSHADOW';
	break;
	case 135:return 'SIZEOF';
	break;
	case 136:return 'CAST';
	break;
	case 137:return 'NAMESPACE';
	break;
	case 138:return 'USING';
	break;
	case 139:return 'LOWP';
	break;
	case 140:return 'MEDIUMP';
	break;
	case 141:return 'HIGHP';
	break;
	case 142:return 'PRECISION';
	break;
	case 143:
		yy.yylval = yy_.yytext;
		return yy.state.classify_identifier(yy.state, yy_.yytext);

		break;
	case 144:return yy_.yytext;
	break;
	case 145:return 'EOF';
	break;
	}
	},
	rules: [/^(?:[ \r\t]+)/,/^(?:[ \t]*#[ \t]*$)/,/^(?:[ \t]*#[ \t]*version\b)/,/^(?:[ \t]*#[ \t]*extension\b)/,/^(?:(^([ \t]*)([ \t]*))line([ \t]+)((([1-9][0-9]*)|([xX][0-9a-fA-F]+)|([0-7]*)))([ \t]+)((([1-9][0-9]*)|([xX][0-9a-fA-F]+)|([0-7]*)))([ \t]*)$)/,/^(?:(^([ \t]*)([ \t]*))line([ \t]+)((([1-9][0-9]*)|([xX][0-9a-fA-F]+)|([0-7]*)))([ \t]*)$)/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)debug([ \t]*)\(([ \t]*)on([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)debug([ \t]*)\(([ \t]*)off([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)optimize([ \t]*)\(([ \t]*)on([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)optimize([ \t]*)\(([ \t]*)off([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)STDGL([ \t]+)invariant([ \t]*)\(([ \t]*)all([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+))/,/^(?:[\n])/,/^(?:.)/,/^(?:\/\/[^\n]*)/,/^(?:[ \t\r]*)/,/^(?::)/,/^(?:[_a-zA-Z][_a-zA-Z0-9]*)/,/^(?:[1-9][0-9]*)/,/^(?:[\n])/,/^(?:[\n])/,/^(?:attribute\b)/,/^(?:const\b)/,/^(?:bool\b)/,/^(?:float\b)/,/^(?:int\b)/,/^(?:break\b)/,/^(?:continue\b)/,/^(?:do\b)/,/^(?:while\b)/,/^(?:else\b)/,/^(?:for\b)/,/^(?:if\b)/,/^(?:discard\b)/,/^(?:return\b)/,/^(?:debugger\b)/,/^(?:bvec2\b)/,/^(?:bvec3\b)/,/^(?:bvec4\b)/,/^(?:ivec2\b)/,/^(?:ivec3\b)/,/^(?:ivec4\b)/,/^(?:vec2\b)/,/^(?:vec3\b)/,/^(?:vec4\b)/,/^(?:mat2\b)/,/^(?:mat3\b)/,/^(?:mat4\b)/,/^(?:in\b)/,/^(?:out\b)/,/^(?:inout\b)/,/^(?:uniform\b)/,/^(?:varying\b)/,/^(?:invariant\b)/,/^(?:flat\b)/,/^(?:smooth\b)/,/^(?:sampler1D\b)/,/^(?:sampler2D\b)/,/^(?:sampler3D\b)/,/^(?:samplerCube\b)/,/^(?:sampler1DShadow\b)/,/^(?:sampler2DShadow\b)/,/^(?:struct\b)/,/^(?:void\b)/,/^(?:layout\b)/,/^(?:\+\+)/,/^(?:--)/,/^(?:<=)/,/^(?:>=)/,/^(?:==)/,/^(?:!=)/,/^(?:&&)/,/^(?:\|\|)/,/^(?:\^\^)/,/^(?:<<)/,/^(?:>>)/,/^(?:\*=)/,/^(?:\/=)/,/^(?:\+=)/,/^(?:%=)/,/^(?:<<=)/,/^(?:>>=)/,/^(?:&=)/,/^(?:\^=)/,/^(?:\|=)/,/^(?:-=)/,/^(?:[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?[fF]?)/,/^(?:\.[0-9]+([eE][+-]?[0-9]+)?[fF]?)/,/^(?:[0-9]+\.([eE][+-]?[0-9]+)?[fF]?)/,/^(?:[0-9]+[eE][+-]?[0-9]+[fF]?)/,/^(?:[0-9]+[fF])/,/^(?:0[xX][0-9a-fA-F]+)/,/^(?:0[0-7]*)/,/^(?:[1-9][0-9]*)/,/^(?:true\b)/,/^(?:false\b)/,/^(?:asm\b)/,/^(?:class\b)/,/^(?:union\b)/,/^(?:enum\b)/,/^(?:typedef\b)/,/^(?:template\b)/,/^(?:this\b)/,/^(?:packed\b)/,/^(?:goto\b)/,/^(?:switch\b)/,/^(?:default\b)/,/^(?:inline\b)/,/^(?:noinline\b)/,/^(?:volatile\b)/,/^(?:public\b)/,/^(?:static\b)/,/^(?:extern\b)/,/^(?:external\b)/,/^(?:interface\b)/,/^(?:long\b)/,/^(?:short\b)/,/^(?:double\b)/,/^(?:half\b)/,/^(?:fixed\b)/,/^(?:unsigned\b)/,/^(?:input\b)/,/^(?:output\b)/,/^(?:hvec2\b)/,/^(?:hvec3\b)/,/^(?:hvec4\b)/,/^(?:dvec2\b)/,/^(?:dvec3\b)/,/^(?:dvec4\b)/,/^(?:fvec2\b)/,/^(?:fvec3\b)/,/^(?:fvec4\b)/,/^(?:sampler2DRect\b)/,/^(?:sampler3DRect\b)/,/^(?:sampler2DRectShadow\b)/,/^(?:sizeof\b)/,/^(?:cast\b)/,/^(?:namespace\b)/,/^(?:using\b)/,/^(?:lowp\b)/,/^(?:mediump\b)/,/^(?:highp\b)/,/^(?:precision\b)/,/^(?:[_a-zA-Z][_a-zA-Z0-9]*)/,/^(?:.)/,/^(?:$)/],
	conditions: {"PRAGMA":{"rules":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145],"inclusive":true},"PP":{"rules":[0,1,2,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145],"inclusive":true},"INITIAL":{"rules":[0,1,2,3,4,5,6,7,8,9,10,11,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145],"inclusive":true}}
	});
	return lexer;
	})();

	function GlslParser() {

		this.jison = parser;
		this.jison.lexer = lexer;
	}

		var proto = GlslParser.prototype;

	proto.parse = function(state) {
		var result;

			this.jison.yy =  {
			test : 1,
			state : state
		};

			try {
			this.jison.parse(state.getTranslationUnit());
		} catch(e) {
			state.addError(e.message, e.lineNumber, e.columnNumber);
			return false;
		}

			return true;
	};

		glsl.parser = new GlslParser();



	glsl.parse = function(src, options) {
		var state,
			result,
			irs
			;

			state = new GlslState(options);
		state.setSource(src);

		result = this.preprocessor.process(state);

		if (result) {
			result = this.parser.parse(state);
		}

			if (result) {
			state.status = true;	
		}

			return state;
	};




	glsl.generate = function(state) {
		var irs,
		    ast,
		    i,
			main
			;

			irs = new Ir(state.options.target);
		ast = state.getAst();

				try {

				for (i = 0; i < ast.length; i++) {
				ast[i].ir(state, irs);
			}

				main = state.symbols.get_function('main');

			if (main.definition.join(",") !== "void") {
				state.addWarning("main() should take no parameters");
			}

				state.symbols.add_variable("<returned>", irs.getTemp(main.getType().slots));

						if (main.type != 'void') {
				state.addWarning("main() should be type void");	
			}

				if (!main) {
				state.addError("main() is not defined");
				return false;
			}

				main.Ast.body.ir(state, irs);

			} catch (e) {

				if (!e.ir) {
				e.message = "compiler error: " + e.message;
			}

				state.addError(e.message, e.lineNumber, e.columnNumber);
			return false;
		}

			state.setIR(irs);

			return true;
	};

	AstNode.prototype.ir_error = function(message) {
		var e = new IrError();

			if (this.location) {
			e.lineNumber = this.location.first_line;
			e.columnNumber = this.location.first_column;
			e.message = message;
		}

			throw e;
	}

	AstNode.prototype.irx = function(state, irs) {
		this.ir_error(util.format("Can't generate ir for %s", this.typeOf()));
	};

	AstTypeSpecifier.prototype.ir = function(state, irs) {

			if (this.is_precision_statement) {
			return;
		}

	};


	AstDeclaratorList.prototype.ir = function(state, irs) {
		var i;

			for (i = 0; i < this.declarations.length; i++) {
			this.declarations[i].ir(state, irs, this.type);
		}
	};

	AstDeclaration.prototype.ir = function(state, irs, type) {
		var qualifier, name, entry, constant, assign, lhs, size;

			if (type.qualifier) {
			qualifier = type.qualifier;
		}

			name = this.identifier;

		entry = state.symbols.add_variable(name);
		entry.type = type.specifier.type_name;
		entry.qualifier = qualifier;

			if (qualifier.indexOf('uniform') !== -1) {
			entry.out = irs.getUniform(entry);
		} else if (qualifier.indexOf('attribute') !== -1) {
			entry.out = irs.getAttribute(entry);
		} else if (qualifier.indexOf('varying') !== -1) {
			entry.out = irs.getVarying(entry);
		} else {
			entry.out = irs.getTemp(entry.getType().slots);
		}

			constant = (qualifier === 'const');

			if (this.is_array) {

						this.array_size.ir(state, irs);

				if (this.array_size.Type != 'int') {
				this.ir_error("array size must be an integer");
			}

				if (!this.array_size.Const) {
				this.ir_error("array size must be constant");
			}

				size = parseInt(this.array_size.Dest);

				if (size < 1) {
				this.ir_error("array size cannot be less than 1");
			}

				entry.size = size;

			entry.base_type = entry.type;
			entry.type += '[]';
		}

			if (this.initializer) {

			if (constant) {
			} else {
				lhs = new AstExpression('ident');
				lhs.primary_expression.identifier = name;
				assign = new AstExpression('=', lhs, this.initializer);
				assign.setLocation(this.location);
				assign.ir(state, irs);
			}

			} else {
			if (constant) {
				this.ir_error("Declaring const without initialier");
			}
		}	
	};

	AstFunctionDefinition.prototype.ir = function(state, irs) {

		this.proto_type.ir(state, irs);

			this.proto_type.entry.Ast = this;
	};


	AstFunction.prototype.ir = function(state, irs) {
		var i;

			if (this.parameters.length == 0) {
			this.entry.definition.push('void');
		}

		for (i = 0; i < this.parameters.length; i++) {
			this.entry.definition.push(this.parameters[i].type.specifier.type_name);
		}
	};


	AstCompoundStatement.prototype.ir = function(state, irs) {
		var i, stmt, retd_entry, maybe_returned;

			retd_entry = state.symbols.get_variable("<returned>");
		maybe_returned = false;

			for (i = 0; i < this.statements.length; i++) {

				stmt = this.statements[i];

				stmt.ir(state, irs);

				if (stmt instanceof AstJumpStatement && stmt.mode == 'return') {

				retd_entry.Passed = true;
				irs.push(new IrInstruction("MOV", retd_entry.out + ".x", "1.0"));
				break;
			}

						if (!maybe_returned && retd_entry.Passed) {
				maybe_returned = true;
				irs.push(new IrInstruction("IF", retd_entry.out + ".x"));
			}
		}

				if (maybe_returned) {
			irs.push(new IrInstruction("ENDIF"));	
		}
	};


	AstExpressionStatement.prototype.ir = function(state, irs) {
		this.expression.ir(state, irs);
	};




	AstExpression.prototype.ir = function(state, irs) {
		var i;

		for (i in this.primary_expression) {
			return this.ir_simple(state, irs);
		}

		if (this.oper) {
			return this.ir_op(state, irs);
		}

		if (this.constructor.name ==  'AstTypeSpecifier') {
			this.Type = this.type_specifier;
			return;
		}

			this.ir_error("Could not translate unknown expression type");
	};



	AstExpression.prototype.ir_op = function(state, irs) {
		var se, temp, ops;

			if (se = this.subexpressions) {
			se[0] ? se[0].ir(state, irs) : null;
			se[1] ? se[1].ir(state, irs) : null;
			se[2] ? se[2].ir(state, irs) : null;
		}

				switch (this.oper) {

			case '=':
				this.ir_assign(state, irs);
				break;

				case 'POS':
				this.Dest = se[0].Dest;
				this.Type = se[0].Type;
				break;

				case 'NEG':

					if (se[0].Dest.substring(0, 1) != '-') {
					this.Dest = "-" + se[0].Dest;	
				} else {
					this.Dest = se[0].Dest.substring(1);	
				}

								this.Type = se[0].Type;

								if (se[0].Const) {
					this.Const = se[0].Const;	
				}

								break;

			case '+':
			case '-':
			case '*':
			case '/':
			case '%':
			case '&':
			case '^':
			case '|':
			case '~':
			case '<<':
			case '>>':
				this.ir_generate(state, irs, 2, true);
				break;

			case '<':
			case '>':
			case '<=':
			case '>=':
			case '==':
			case '!=':
			case '&&':
			case '^^':
			case '||':
				this.ir_generate(state, irs, 2);
				break;
			case '!':
				this.ir_generate(state, irs, 1);
				break;


			case '++x':
			case '--x':
			case 'x++':
			case 'x--':
				this.ir_incdec(state, irs);
				break;
			case '[]':
				this.ir_arr_index(state, irs);
				break;
			default:
				this.ir_error(util.format("Could not translate unknown expression %s (%s)", this, this.oper));
		}
	};


	AstExpression.prototype.ir_assign = function(state, irs, skip_comment) {
		var cond, ir, temp, size, slots, swz, i, entry, lhs, rhs, com;

			lhs = this.subexpressions[0];
		rhs = this.subexpressions[1];

			if (lhs.Type != rhs.Type || rhs.Const) {
			this.ir_cast.apply(rhs, [state, irs, lhs.Type]);
		}

			this.Type = lhs.Type;

			if (lhs.Entry && lhs.Entry.constant) {
			this.ir_error(util.format("Cannot assign value to constant %s", lhs.Dest));
		}

			if (!skip_comment) {
			com = util.format("%s => %s %s <%s>", rhs.Dest, lhs.Type, lhs.Dest, lhs.toString());
			irs.push(new IrComment(com, this.location));
		}

			size = types[this.Type].size;
		slots = types[this.Type].slots;

		swz = Ir.swizzles[0].substring(0, 4 - (((slots * 4) - size) / slots));

		if (swz == Ir.swizzles[0]) {
			swz = "";
		}

			for (i = 0; i < slots; i++) {
				ir = new IrInstruction('MOV', lhs.Dest, rhs.Dest);
				ir.addOffset(i);
				ir.setSwizzle(swz);
				irs.push(ir);
		}
	};


	AstExpression.prototype.ir_cast = function(state, irs, type) {

		if (Type.canCast(this.Type, type)) {

			if (this.Const) {
				this.Dest = Type.castTo(this.Dest, this.Type, type);
				this.Type = type;
			} else {
				this.ir_error(util.format("Could not assign value of type %s to %s", this.Type, type));
			}

			} else {
			this.ir_error(util.format("Could not assign value of type %s to %s", this.Type, type));
		}
	};

	AstExpression.prototype.ir_simple = function(state, irs) {
		var name, entry, t;

			if (this.oper == '.') {
			this.ir_field(state, irs);
			return;
		}

		if (name = this.primary_expression.identifier) {

			entry = state.symbols.get_variable(name) || state.symbols.get_function(name);

				if (!entry ) {
				this.ir_error(util.format("%s is undefined", name));
			}

				this.Type = entry.type;
			this.Entry = entry;

				if (entry.constant) {
				this.Dest = entry.constant;
			} else {
				this.Dest = entry.out;
			}

				return;
		}

		if (this.primary_expression.type == 'float') {
			this.Type = 'float';
			this.Dest = this.primary_expression.float_constant;
			this.Const = true;
			return;
		}

		if (this.primary_expression.type == 'int') {
			this.Type = 'int';
			this.Dest = this.primary_expression.int_constant;
			this.Const = true;
			return;
		}

			this.ir_error("Cannot translate unknown simple expression type");
	};

	AstExpression.prototype.ir_generate = function(state, irs, len, arith) {
		var table, se, oprd_types, dest, i, j, def, match, comment, cnst;

			if (!(table = builtin.oper[this.oper])) {
			this.ir_error(util.format("Could not generate operation %s", this.oper));
		}

			se = this.subexpressions;

		if (state.options.opt.fold_constants && arith) {
			if (se[0].Const && se[1].Const) {

					cnst = eval(se[0].Dest + this.oper + se[1].Dest);

				if (Number.isFinite(cnst)) {
					this.Dest = "" + cnst;
					this.Type = 'float';
					this.Const = true;
					return;
				}
			}
		}

			oprd_types = [];
		dest = [];

			for (i = 0; i < len; i++) {
			oprd_types.push(se[i].Type);
			dest.push(se[i].Dest);
		}

			def = new RegExp(oprd_types.join(",") + "\:(.*)");
		for (j in table) {
			if (match = j.match(def)) {
				break;
			}
		}

			if (!match) {
			this.ir_error(util.format("Could not apply operation %s to %s", this.oper, oprd_types.join(", ")));
		}

			this.Type = match[1];
		this.Dest = irs.getTemp(types[this.Type].slots);

				dest.splice(0, 0, this.Dest);

			if (len <= 4) {
		}

			if (len == 1) {
			comment = util.format("(%s %s %s) => %s %s", this.oper, se[0].Type, se[0].Dest, this.Type, this.Dest);
		} else if (len == 2) {
			comment = util.format("(%s %s %s %s %s) => %s %s", se[0].Type, se[0].Dest, this.oper, se[1].Type, se[1].Dest, this.Type, this.Dest);
		} else if (len == 3) {
			comment = util.format("(%s %s ? %s %s : %s %s) => %s %s", se[0].Type, se[0].Dest, se[1].Type, se[1].Dest, se[2].Type, se[2].Dest, this.Type, this.Dest);
		}

			irs.push(new IrComment(comment, this.location));

			irs.build(table[j], dest);
	};

	AstExpression.prototype.ir_incdec = function(state, irs) {
		var se, op, ins, post, type, i, ir;

			se = this.subexpressions[0];

			op = this.oper.replace('x', '');
		ins = op === '++' ? 'ADD' : 'SUB';
		post = this.oper.indexOf('x') === 0;
		type = types[se.Type];

		if (type.base != 'int' && type.base != 'float') {
			this.ir_error(util.format("Could not apply operation %s to %s", op, se.Type));
		}

			this.Type = se.Type;

			if (post) {
			this.Dest = irs.getTemp(type.slots);
		} else {
			this.Dest = se.Dest;
		}

			irs.push(new IrComment(util.format("(%s%s) => %s %s", post ? se.Dest : op, post ? op : se.Dest, this.Type, this.Dest), this.location));

				for (i = 0; i < type.slots; i++) {

				if (post) {
				this.Dest = irs.getTemp(type.slots);
				ir = new IrInstruction('MOV', this.Dest, se.Dest);
				ir.addOffset(i);
				ir.setSwizzle(type.swizzle);
				irs.push(ir);
			}

				ir = new IrInstruction(ins, se.Dest, se.Dest, "1.0");
			ir.addOffset(i);
			ir.setSwizzle(type.swizzle);
			irs.push(ir);
		}

		};

	AstExpression.prototype.ir_arr_index = function(state, irs) {
		var arr, idx, entry, size, cnst, oprd;

				arr = this.subexpressions[0];
		idx = this.subexpressions[1];

				entry = arr.Entry;

		if (idx.Type != 'int') {
			this.ir_error("array index out of bounds");
		}

		if (!entry.size) {
			this.ir_error("cannot index a non-array value");	
		}

		if (types[entry.base_type].slots > 1) {
			this.ir_error("array indexing for matrices not implemented yet");	
		}

			this.Type = entry.base_type;

		if (idx.Const) {

				cnst = parseInt(idx.Dest);

				if (cnst < 0 || cnst >= entry.size) {
				this.ir_error("array index out of bounds");	
			}

				oprd = new IrOperand(arr.Dest);
			oprd.index = cnst;

					this.Dest = oprd.toString();

			} else {


				this.ir_error("variable indexing not implemented yet");	
		}
	};

	AstFunctionExpression.prototype.ir = function(state, irs) {
		var i, e, name, entry, ret_entry, retd_entry, call_types, operands, param, proto, loc;

			if (this.cons) {
			return this.ir_constructor(state, irs);
		}

			name = this.subexpressions[0].primary_expression.identifier;

			operands = [];
		call_types = [];

				for (i = 0; i < this.expressions.length; i++) {

				e = this.expressions[i];
			e.ir(state, irs);

				call_types.push(e.Type);
			operands.push(e.Dest);
		}

			entry = state.symbols.get_function(name, call_types);
		if (!entry) {
			this.ir_error(util.format("Function %s(%s) is not defined", name, call_types.join(", ")));
		}

			this.Type = entry.type;
		this.Dest = irs.getTemp(entry.getType().slots);

			irs.push(new IrComment(util.format("%s(%s) => %s %s", name, operands.join(", "), this.Type, this.Dest), this.location));

			if (entry.code) {

			operands.unshift(this.Dest);
			irs.build(entry.code, operands);

					} else if (entry.Ast) {

			state.symbols.push_scope();

			proto = entry.Ast.proto_type;
			for (i = 0; i < proto.parameters.length; i++) {
				param = proto.parameters[i];
				loc = state.symbols.add_variable(param.identifier, param.type.specifier.type_name);
				loc.out = irs.getTemp(loc.getType().slots);

				irs.push(new IrComment(util.format("PARAM %s => %s %s", operands[i], loc.out, param.type.specifier.type_name), param.location));

				lhs = new AstExpression('<param>');
				lhs.setLocation(this.getLocation());
				lhs.Type = loc.type;
				lhs.Dest = loc.out;

					assign = new AstExpression('=', lhs, this.expressions[i]);
				assign.setLocation(this.getLocation());
				assign.ir_assign(state, irs, true);
			}

			ret_entry = state.symbols.add_variable("<return>", this.Type);
			ret_entry.out = this.Dest;

				retd_entry = state.symbols.add_variable("<returned>", "bool");
			retd_entry.out = irs.getTemp(retd_entry.getType().slots);

				entry.Ast.body.ir(state, irs);

				state.symbols.pop_scope();
		}
	};


	AstFunctionExpression.prototype.ir_constructor = function(state, irs) {
		var type, comment_text, comment, i, expr, src_expr, src_i, src_c, oprd, dest;

			type = this.subexpressions[0].type_specifier;

			this.Type = type.name;
		this.Dest = irs.getTemp(type.slots);

			comment_text = [];
		comment = new IrComment("", this.location);
		irs.push(comment);

		for (i = 0; i < this.expressions.length; i++) {

						expr = this.expressions[i];

				if (expr) {
				expr.ir(state, irs);	
				comment_text.push(expr.Dest);
			}

					}

			src_expr = this.expressions[0];
		src_i = 0; 
		src_c = 0; 

			for (dest_i = 0; dest_i < type.size; dest_i++) {

				if (!src_expr) {
				this.ir_error("Not enough parameters to constructor");				
			}

			if (types[src_expr.Type].size > 4) {
				this.ir_error("Matrix components not implemented yet");	
			}

			dest = util.format("%s.%s", this.Dest, Ir.swizzles[0][dest_i]);

			oprd = new IrOperand(src_expr.Dest);

				if (!oprd.swizzle) {
				oprd.swizzle = Ir.swizzles[0][src_c];
			}

				irs.push(new IrInstruction('MOV', dest, oprd.toString()));

				src_c++;

			if (src_c >= types[src_expr.Type].size) {
				if (this.expressions[src_i + 1]) {
					src_i++;
					src_expr = this.expressions[src_i];
					src_c = 0;
				}
			}

			}

			comment.comment = util.format("%s(%s) => %s %s", this.Type, comment_text.join(", "), this.Type, this.Dest);
	};


	AstExpression.prototype.ir_field = function(state, irs) {
		var field, swz, base, se;

		field = this.primary_expression.identifier;

			se = this.subexpressions[0];
		se.ir(state, irs);

			if (Ir.isSwizzle(field)) {

				base = types[se.Type].base;
			if (field.length > 1) {
				if (base == 'int') {
					base = 'ivec' + field.length;	
				}
				if (base == 'bool') {
					base = 'bvec' + field.length;	
				}
				if (base == 'float') {
					base = 'vec' + field.length;	
				}
			}

				this.Type = base;

				if (field.length > 4 || !this.Type) {
				this.ir_error(util.format("Invalid field selection %s.%s", se, field));
			}

				this.Dest = util.format("%s.%s", se.Dest, Ir.normalizeSwizzle(field));
		}
	}


	AstSelectionStatement.prototype.ir = function(state, irs) {
		var ir, cond;

			this.condition.ir(state, irs);

			irs.push(new IrComment(util.format("if %s then", this.condition.Dest), this.location));

		ir = new IrInstruction('IF', this.condition.Dest);

			if (['bool', 'int', 'float'].indexOf(this.condition.Type) === -1) {
			this.ir_error("boolean expression expected");
		}

			if (!ir.d.swizzle) {
			ir.d.swizzle = 'x';
		}

			irs.push(ir);

			this.then_statement.ir(state, irs);

			if (this.else_statement) {

				irs.push(new IrInstruction('ELSE'));

				this.else_statement.ir(state, irs);
		}

			irs.push(new IrInstruction('ENDIF'));
	}

	AstJumpStatement.prototype.ir = function(state, irs) {
		var ret, ret_entry, assign, lhs;

			switch (this.mode) {

				case 'return':

					ret = this.opt_return_value;

							if (ret) {

										ret.ir(state, irs);

								ret_entry = state.symbols.get_variable('<return>');


								irs.push(new IrComment(util.format("return => %s %s", ret.Dest, ret.Type), this.location));

					lhs = new AstExpression('<return>');
					lhs.setLocation(this.getLocation());
					lhs.Type = ret.Type;
					lhs.Dest = ret_entry.out;

								assign = new AstExpression('=', lhs, ret);
					assign.setLocation(this.getLocation());
					assign.ir_assign(state, irs, true);

							} else {
					irs.push(new IrComment("return", this.location));
				}

							break;

						case 'debugger':

								irs.push(new IrComment("debugger", this.location));
				irs.push(new IrInstruction("DBGR"));
				break;

						default:
		}

		};




	function Ir(target) {

			this.target = target;

			this.symbols = {
			uniform : {
				next : 0,
				entries : {}
			},
			attribute : {
				next : 0,
				entries : {}
			},
			varying : {
				next : 0,
				entries : {}
			},
			temp : {
				next : 0
			}
		};

			this.code = [];
		this.last = null;
	}

		Ir.prototype.getTemp = function(n) {
		var t;

			n = n || 1;
		t = 'temp@' + this.symbols.temp.next;

			this.symbols.temp.next += n;

			return t;
	};

	Ir.prototype.getUniform = function(entry) {

			var table = this.symbols.uniform, out;

			if (!table.entries[entry.name]) {
			table.entries[entry.name] = entry;
			entry.out = 'uniform@' + table.next;
			table.next += entry.getType().slots;
		}

			return entry.out;
	};

	Ir.prototype.getAttribute = function(entry) {

			var table = this.symbols.attribute, out;

			if (!table.entries[entry.name]) {
			table.entries[entry.name] = entry;
			entry.out = 'attribute@' + table.next;
			table.next += entry.getType().slots;
		}

			return entry.out;
	};

	Ir.prototype.getVarying = function(entry) {

			var table = this.symbols.varying, out;

			if (!table.entries[entry.name]) {
			table.entries[entry.name] = entry;
			entry.out = 'varying@' + table.next;
			table.next += entry.getType().slots;
		}

			return entry.out;
	};


			Ir.prototype.get = function(i) {
		return this.code[i];	
	};

		Ir.prototype.push = function(ir) {
		this.code.push(ir);
		this.last = ir;
	};

		Ir.isSwizzle = function(swz) {

			if (swz.match(/[xyzw]+/)) {
			return true;	
		}

			if (swz.match(/[rgba]+/)) {
			return true;	
		}

			if (swz.match(/[stpq]+/)) {
			return true;	
		}
	};

		Ir.normalizeSwizzle = function(swz) {
		var n;

			if (!this.isSwizzle(swz)) {
			return null;
		}

				n = swz
		   .replace(/[rs]/g, 'x')
		   .replace(/[gt]/g, 'y')
		   .replace(/[bp]/g, 'z')
		   .replace(/[aq]/g, 'w')
		   ;

			return n;
	};

		Ir.swizzles = ["xyzw", "rgba", "stpq"];


	Ir.prototype.replaceName = function(start, old, nw, index, repl) {
		var i, j, ir, f, name, neg_const;
		neg_const = old.match(/^\-([0-9]+\.[0-9]+)/);
		if (neg_const) {
			old = neg_const[1];
			neg_const = true;
		}

			for (i = start; i < this.code.length; i++) {
			ir = this.code[i];

			for (j = 0; j < IR.operands.length; j++) {
				f = IR.operands[j];
				if (ir[f] && ir[f].name == old) {
					if (repl) {
						ir[f] = new Ir.Operand(ir[f].neg + nw);
					} else {
						ir[f].name = nw;
						ir[f].addOffset(index);
					}
					if (neg_const && ir[f].neg) {
						ir[f].neg = "";
					}
				}	
			}

					}
	};

			Ir.prototype.toString = function() {
		return this.code.join("\n");
	};


	Ir.prototype.build = function(code, oprds) {
		var dest, i, j, k, o, n, t, oprd, ir, new_swz, temps;

		for (i = 0; i < oprds.length; i++) {

				oprd = new IrOperand(oprds[i]);

				if (oprd.swizzle) {

				new_swz = Ir.swizzles[0].substring(0, oprd.swizzle.length);

					if (oprd.swizzle != new_swz) {
					dest = this.getTemp();
					ir = new IrInstruction('MOV', util.format("%s.%s", dest, new_swz), oprd.full);
					this.push(ir);
					oprd = new IrOperand(dest);
				}
			}

				oprds[i] = oprd;
		}

			temps = [];

		for (i = 0; i < code.length; i++) {

				ir = new IrInstruction(code[i]);

			for (j = 0; j < IrInstruction.operands.length; j++) {		

								o = IrInstruction.operands[j];
				oprd = ir[o];

								if (!oprd) {
					break;
				}

				n = oprd.name.match(/%(\d+)/);
				if (n) {
					n = parseInt(n[1]);
					ir[o] = new IrOperand(oprds[n - 1].toString());
					ir[o].addOffset(oprd.address);
					ir[o].swizzle = oprd.swizzle;
					ir[o].neg = oprd.neg;
				}

				t = oprd.name.match(/%t(\d+)/);
				if (t) {

					t = parseInt(t[1]);
					while (temps.length < t) {
						temps.push(this.getTemp());	
					}
					t = temps[t - 1].split('@');

										oprd.name = t[0];
					oprd.address = t[1];
					oprd.full = oprd.toString();
				}

							}

				this.push(ir);
		}
	};


	function IrError(msg) {
		this.msg = msg;
		this.ir = true;
	}
	IrError.prototype = Error.prototype;








	function IrInstruction(op, d, s1, s2, s3) {
		var args;

			this.str = null;
		this.line = null;

			if (arguments.length == 1) {
			args = op.split(/[\s,]/);
			op = args[0];
			d = args[1];
			s1 = args[2];
			s2 = args[3];
			s3 = args[4];
		}

			this.op = op;
		this.d = this.operand(d);
		this.s1 = this.operand(s1);
		this.s2 = this.operand(s2);
		this.s3 = this.operand(s3);
	}

		IrInstruction.operands = ['d', 's1', 's2', 's3'];


			IrInstruction.prototype.operand = function(opr) {
		return opr ? new IrOperand(opr) : "";
	};

	IrInstruction.prototype.addOffset = function(offset) {
		var i, o;

			for (i = 0; i < IrInstruction.operands.length; i++) {
			o = IrInstruction.operands[i];
			if (this[o]) {
				this[o].addOffset(offset);	
			}
		}
	};

	IrInstruction.prototype.setSwizzle = function(swz) {
		var i, o;

			for (i = 0; i < IrInstruction.operands.length; i++) {
			o = IrInstruction.operands[i];
			if (this[o] && !this[o].swizzle) {
				this[o].swizzle = swz;
			}
		}
	};

	IrInstruction.prototype.toString = function() {
		var out;
		out = util.format("%s%s%s%s%s;",
			this.op,
			this.d  ? ' '  + this.d  : '',
			this.s1 ? ', ' + this.s1 : '',
			this.s2 ? ', ' + this.s2 : '',
			this.s3 ? ', ' + this.s3 : ''
			);
		return out;
	};

	function IrComment(comment, loc) {
		this.comment = comment;
		this.loc = loc;
	}

		IrComment.prototype.toString = function() {
		var c = this.comment;

			if (this.loc) {
			c = util.format("%s [%s:%s-%s:%s]", c, this.loc.first_line, this.loc.first_column, this.loc.last_line, this.loc.last_column);
		}
		c = "\n# " + c;

			return c;
	};


	function IrOperand(str, raw) {

			this.full = "";
		this.neg = "";
		this.name = "";
		this.address = "";
		this.swizzle = "";
		this.number = "";
		this.raw = "";
		this.index = "";

			if (raw) {
			this.full = str;
			this.raw = str;
		} else {
			this.parse(str);
		}
	}

	IrOperand.prototype.parse = function(str) {
		var parts, regex;

			if (!str) {
			return;
		}

			if (!isNaN(parseFloat(str))) {
			this.raw = str;
			return;
		}

		regex = "(\-)?";

		regex += "([\\w%]+)";

		regex += "(?:@(\\d+))?";

		regex += "(?:\\[(\\d+)\\])?";

		regex += "(?:\\.([xyzw]+))?";

			regex = new RegExp("^" + regex + "$");

			if (parts = str.match(regex)) {

				this.neg = parts[1] || "";
			this.name = parts[2];
			this.address = parseInt(parts[3]) || 0;
			this.index = parseInt(parts[4]) || 0;
			this.swizzle = parts[5] || "";
		} else {
			if (parts = str.match(/^"(.*)"$/)) {
				this.raw = parts[1];
			} else {
				this.raw = str;
			}
		}

			this.full = this.toString();
	};

	IrOperand.prototype.addOffset = function(offset) {

			this.address = this.address || 0;

			this.address += offset;
	};

	IrOperand.prototype.toString = function() {
		var str;

			if (this.raw) {
			str = this.raw;	
		} else {
			str = this.neg + this.name + ("@" + this.address) + (this.index !== "" ? "[" + this.index + "]" : "") + (this.swizzle ? "." + this.swizzle : "");
		}

				return str;
	};








	function GlslProgramJavascript() {

			this.vertex_code = [];
		this.fragment_code = [];

			this.symbols = new GlslProgramJavascriptVars();
		this.context = new GlslProgramJavascriptContext();

			this.library = {
			tex : function(dest, i, sampler, src, j, dim) {
				dest[i] = 0;
				dest[i + 1] = 0;
				dest[i + 2] = 0;
				dest[i + 3] = 1;
			}
		};

			this.vertex = null;
		this.shader = null;
	}

		var proto = GlslProgramJavascript.prototype;

		GlslProgramJavascript.translation_table = {
		'ABS'  : '%1.* = Math.abs(%2.*);',
		'ADD'  : '%1.* = %2.* + %3.*;',
		'AND'  : '%1.* = %2.* & %3.*;',
		'CEIL' : '%1.* = Math.ceil(%2.*);',
		'CMP'  : '%1.* = (%2.* < 0.0) ? %3.* : %4.*;',
		'COS'  : '%1.* = Math.cos(%2.*);',
		'DIV'  : '%1.* = %2.* / %3.*;',
		'DBGR' : 'debugger;',
		'DP2'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y);',
		'DP3'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y) + (%2.z * %3.z);',
		'DP4'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y) + (%2.z * %3.z) + (%2.w * %3.w);',
		'ELSE' : '} else {',
		'ENDIF': '}',
		'FLR'  : '%1.* = Math.floor(%2.*);',
		'FRC'  : '%1.* = %2.* - Math.floor(%2.*);',
		'IF'   : 'if (%1.x) {',
		'MAD'  : '%1.* = (%2.* * %3.*) + %4.*;',
		'MAX'  : '%1.* = Math.max(%2.*, %3.*);',
		'MIN'  : '%1.* = Math.min(%2.*, %3.*);',
		'MOD'  : '%1.* = %2.* % %3.*;',
		'MOV'  : '%1.* = %2.*;',
		'MUL'  : '%1.* = %2.* * %3.*;',
		'OR'   : '%1.* = %2.* | %3.*;',
		'POW'  : '%1.x = Math.pow(%2.x, %3.x);',
		'RET'  : 'return;',
		'RSQ'  : '%1.* = (1.0 / Math.sqrt(%2.*));',
		'SEQ'  : '%1.* = (%2.* === %3.*) ? 1.0 : 0.0;',
		'SGE'  : '%1.* = (%2.* >=  %3.*) ? 1.0 : 0.0;',
		'SGT'  : '%1.* = (%2.* >   %3.*) ? 1.0 : 0.0;',
		'SIN'  : '%1.* = Math.sin(%2.*);',
		'SLE'  : '%1.* = (%2.* <=  %3.*) ? 1.0 : 0.0;',
		'SLT'  : '%1.* = (%2.* <   %3.*) ? 1.0 : 0.0;',
		'SNE'  : '%1.* = (%2.* !== %3.*) ? 1.0 : 0.0;',
		'SUB'  : '%1.* = %2.* - %3.*;',
		'TAN'  : '%1.* = Math.tan(%2.*);', 
		'TEX'  : 'tex(%1, %4, %2, %5, %3.x, 0);', 
		'XOR'  : '%1.* = %2.* ^ %3.*;'
	}; 

	proto.toString = function(target) {

			if (target === glsl.target.fragment) {
			return this.fragment_code.join("\n");	
		} else if (target === glsl.target.vertex) {
			return this.vertex_code.join("\n");	
		} else {
			return this.current.join("\n");
		}
	};

	proto.addObjectCode = function(object, target) {
		var i, errors;


			this.mergeSymbols(object);

			this.current = [];

			for (i = 0; i < object.code.length; i++) {
			try {
				this.instruction(object.code[i]);
			} catch (e) {
				this.error = util.format("%s at %s:%s", e.message, e.lineNumber, e.columnNumber);
				return false;
			}
		}

			if (target == glsl.target.vertex) {
			this.vertex_code = this.current;
		} else if (target == glsl.target.fragment) {
			this.fragment_code = this.current;
		}

			return true;
	};

	proto.mergeSymbols = function(object) {
		var s, t, n, entry, sym, start, slots, comp;

				for (s in object.symbols) {

				t = object.symbols[s].entries;	

				for (n in t) {

					entry = t[n];
				start = parseInt(entry.out.split('@')[1]);
				slots = entry.getType().slots;
				comp = entry.getType().size / slots;

					if (s == 'uniform') {

										sym = this.symbols.addUniform(entry.name, start, slots, comp);

										if (this.findSymbolCollision(this.symbols.uniform, sym)) {
						this.rewriteSymbol(this.symbols.uniform, sym, object);
					}

					} else if (s == 'attribute') {					
					this.symbols.addAttribute(entry.name, start, slots, comp);
				} else if (s == 'varying') {
					this.symbols.addVarying(entry.name, start, slots, comp);				
				}

				}
		}
	};

	proto.findSymbolCollision = function(table, symbol) {
		var i, my_start, my_end, start, end;

			my_start = symbol.pos;
		my_end = my_start + symbol.slots - 1;

			for (i in table) {

				if (i == symbol.name) {
				continue;	
			}

						start = table[i].pos;
			end = start + table[i].slots - 1;

						if ((my_start >= start && my_start <= end) || (my_end >= start && my_end <= end)) {
				return true;
			}

					}

			return false;
	};

	proto.findNewSymbolPosition = function(table, symbol) {
		var i, size, addresses, last, next;

			addresses = [];

		for (i in table) {

						if (symbol.name == i) {
				continue;	
			}

			addresses.push(table[i].pos);

			addresses.push(table[i].pos + table[i].slots - 1);
		}

				addresses.sort();

		if (addresses[0] >= symbol.slots) {
			return 0;
		}

		for (i = 1; i < addresses.length; i += 2) {		
			last = addresses[i];
			next = addresses[i];

						if (next - last - 1 > symbol.slots) {
				return last + 1;	
			}
		}


			return addresses.slice(-1)[0] + 1;
	};

	proto.rewriteSymbol = function(table, symbol, object) {
		var pos, old_start, old_end, diff, i, ins;

			old_start = symbol.pos;
		old_end = old_start + symbol.slots - 1;

			symbol.pos = this.findNewSymbolPosition(table, symbol);
		diff = symbol.pos - old_start;

			for (i = 0; i < object.code.length; i++) {

				ins = object.code[i];

						if (!(ins instanceof IrInstruction)) {
				continue;	  
			}

				this.rewriteOperandAddress(ins.d, old_start, old_end, diff, symbol);
			this.rewriteOperandAddress(ins.s1, old_start, old_end, diff, symbol);
			this.rewriteOperandAddress(ins.s2, old_start, old_end, diff, symbol);
			this.rewriteOperandAddress(ins.s3, old_start, old_end, diff, symbol);
		}
	};

	proto.rewriteOperandAddress = function(oprd, old_start, old_end, diff, symbol) {
		var diff;

				if (!oprd) {
			return;	
		}

			if (oprd.name != symbol.type) {
			return;
		}

			if (oprd.address >= old_start && oprd.address <= old_end) {
			oprd.address += diff;
		}
	};

	proto.build = function() {

			var module, shaders;

			module = new Function("stdlib", "foreign", "heap",
			"//\"use asm\";\n" +
			"var\n" +
			"uniform_f32   = new stdlib.Float32Array(heap,   0, 128),\n" +
			"attribute_f32 = new stdlib.Float32Array(heap, 512, 128),\n" +
			"varying_f32   = new stdlib.Float32Array(heap, 1024, 128),\n" +
			"result_f32    = new stdlib.Float32Array(heap, 1536, 128),\n" +
			"temp_f32      = new stdlib.Float32Array(heap, 2048, 128),\n" +
			"jstemp        = new stdlib.Float32Array(heap, 2544,   4),\n" +
			"tex           = foreign.tex;\n" +
			";\n" +
			"function vs() {\n" +
				this.vertex_code.join("\n") + "\n" +
			"}\n" +
			"function fs() {\n" +
				this.fragment_code.join("\n") + "\n" +
			"}\n" +
			"return { fragment : fs, vertex : vs };"
		);

			shaders = module(window, this.library, this.context.heap);

			this.vertex = shaders.vertex;
		this.fragment = shaders.fragment;	
	};

	proto.instruction = function(ins) {
		var tpl, dest, src, i, j, k, code, js;

			if (ins instanceof IrComment) {
			this.current.push('// ' + ins.toString().replace("\n", ""));
			return;
		}

			this.current.push('// ' + ins.toString());

			if (!(tpl = GlslProgramJavascript.translation_table[ins.op])) {
			throw new Error(util.format("Could not translate opcode '%s'", ins.op));
		}

		dest = this.buildComponents(ins.d, true);

			if (!dest) {
			this.current.push(tpl);
			return;
		}

			src = [];
		src.push(this.buildComponents(ins.s1));
		src.push(this.buildComponents(ins.s2));
		src.push(this.buildComponents(ins.s3));

			if (ins.op == 'TEX') {
			js = tpl.replace(/%1/g, dest.name);
			js = js.replace(/%2/g, src[0].name);
			js = this.replaceOperand(js, '%3', src[1], 0);
			js = js.replace(/%4/g, dest.start);
			js = js.replace(/%5/g, src[0].start);

				this.current.push(js);
			this.current.push("");
			return;
		}

			this.generateTemp(dest, src, tpl);

			for (i = 0; i < dest.components.length; i++) {

				js = this.replaceOperand(tpl, '%1', dest, i);

				for (j = 0; j < 3; j++) {

								if (src[j]) {
					js = this.replaceOperand(js, '%' + (j + 2), src[j], i);
				}

				}

				this.current.push(js);
		}

			this.current.push("");
	};


	proto.replaceOperand = function(tpl, from, op, n) {
		var i,
		    out,
		    name,
		    addr,
		    swz = ['x', 'y', 'z', 'w']
			;

			if (op.raw) {
			name = op.name;
		} else {
			if (op.jstemp && op.jstemp[n]) {
				name = 'jstemp';
				addr = n;
			} else {
				name = op.name;
				if (op.components) {
					addr = op.start + op.components[n];
				}
			}
		}

			if (op.components) {
			out = tpl.replace(from + '.*', util.format("%s[%s]", name, addr));
		} else {
			out = tpl.replace(from + '.*', name);
		}

			for (i = 0; i < swz.length; i++) {
			out = out.replace(new RegExp(from + '\.' + swz[i], 'g'), util.format("%s[%s]", name, op.start + i));
		}

			return out;
	};


	proto.buildComponents = function(opr, dest) {
		var i, swz, out;

			if (!opr) {
			return null;	
		}

			out = {};

			if (opr.raw) {
			out.name = opr.raw;
			out.raw = true;
			return out;
		}

			out.name = opr.neg + opr.name + '_f32';
		out.start = 4 * opr.address + 4 * opr.index;
		out.components = [];
		out.jstemp = [];

		swz = opr.swizzle || "xyzw";
		swz = swz.split("");

			for (i = 0; i < 4; i++) {
			if (swz.length <= i) {
				if (!dest) {
					out.components.push(out.components[i - 1]);	
					out.jstemp.push(null);
				}
			} else {
				out.components.push("xyzw".indexOf(swz[i]));
				out.jstemp.push(null);
			}
		}

			return out;
	};

		proto.generateTemp = function(dest, src, tpl) {
		var i,
		    c,
			op,
		    written
			;

				for (i = 0; i < dest.components.length; i++) {
			written = dest.components.slice(0, i);

				for (c = 0; c < src.length; c++) {
				op = src[c];
				if (op && op.name == dest.name && op.start == dest.start && written.indexOf(op.components[i]) != -1) {
					op.jstemp[i] = true;
					this.current.push(util.format("jstemp[%s] = %s[%s]", i, op.name, op.start + op.components[i]));
				}
			}
		}

	};

	proto.getUniformLocation = function(name) {

			if (this.symbols.uniform[name]) {
			return this.symbols.uniform[name].start;	
		}

			return false;
	};

	proto.getUniformSize = function(name) {

			if (this.symbols.uniform[name]) {
			return this.symbols.uniform[name].size;	
		}

				return false;
	};

	proto.setUniformData = function(name, data) {
		var i, l, s, d;

				d = data.length;
		l = this.getUniformSize(name);
		s = this.getUniformLocation(name);

				if (l === false) {
			return;	
		}

				this.context.uniform_f32.set(data, i + s);
	};

	proto.getAttributeLocation = function(name) {

			if (this.symbols.attribute[name]) {
			return this.symbols.attribute[name].start;	
		}

				return false;
	};

	proto.getAttributeSize = function(name) {

			if (this.symbols.attribute[name]) {
			return this.symbols.attribute[name].size;	
		}

				return false;
	};

	proto.setAttributeData = function(name, data) {
		var i, l, s, d;

				d = data.length;
		l = this.getAttributeSize(name);
		s = this.getAttributeLocation(name);

				if (l === false) {
			return;	
		}

				this.context.attribute_f32.set(data, i + s);
	};

	proto.getResultData = function(start, size) {
		var res;
		res = Array.prototype.slice.apply(this.context.result_f32, [start, size]);
		return res;
	};

	proto.setTexFunction = function(func) {
		this.library.tex = func;
	};


			glsl.program = GlslProgramJavascript;






	function GlslProgramJavascriptContext() {

			this.heap = new ArrayBuffer(640 * 4);

			this.uniform_f32 = new Float32Array(this.heap, 0, 128);
		this.attribute_f32 = new Float32Array(this.heap, 128 * 4, 128);
		this.varying_f32 = new Float32Array(this.heap, 256 * 4, 128);
		this.result_f32 = new Float32Array(this.heap, 384 * 4, 128);
	}

		var proto = GlslProgramJavascriptContext.prototype;





	function GlslProgramJavascriptVars() {
		this.uniform = {};
		this.attribute = {};
		this.varying = {};
	}

		var proto = GlslProgramJavascriptVars.prototype;


	proto.addUniform = function(name, pos, slots, comp) {

			this.uniform[name] = new GlslProgramJavascriptVar(name, pos, slots, comp, 'uniform');

				return this.uniform[name];	
	};

	proto.addAttribute = function(name, pos, slots, comp) {

			this.attribute[name] = new GlslProgramJavascriptVar(name, pos, slots, comp, 'attribute');

			return this.attribute[name];	
	};

	proto.addVarying = function(name, pos, slots, comp) {

			this.varying[name] = new GlslProgramJavascriptVar(name, pos, slots, comp, 'varying');

			return this.varying[name];
	};



	function GlslProgramJavascriptVar(name, pos, slots, comp, type) {
		this.name = name;
		this.pos = pos;
		this.slots = slots;
		this.components = comp;
		this.type = type;
	}




			this.glsl = glsl;

	if (typeof module !== 'undefined') {
		module.exports = glsl;
	}

}());



(function() {

	var proto;

	var GPU = {};


		var util = {};

		(function(exports) {

	exports.inherits = function(ctor, superCtor) {
	  ctor.super_ = superCtor;
	  ctor.prototype = Object.create(superCtor.prototype, {
	    constructor: {
	      value: ctor,
	      enumerable: false,
	      writable: true,
	      configurable: true
	    }
	  });
	};

		}(util));



		GPU.initialize = function() {
		this.renderer = new cnvgl_renderer();
	};




			function BaseBuffer(size, type) {

			this.size = size;

			this.data = new type(size);

			this.id = Objects.add(this);
	};



	function ColorBuffer(width, height) {

			if (!height) {
			height = 1;
		}

			this.width = width;
		this.height = height;

			BaseBuffer.apply(this, [this.width * this.height * 4, Uint8ClampedArray]);
	}

		util.inherits(ColorBuffer, BaseBuffer);

		var proto = ColorBuffer.prototype;

	proto.clear = function(color) {
		this.rect(0, 0, this.width - 1, this.height - 1, color);	
	};

	proto.rect = function(x1, y1, x2, y2, color) {
		var i, j, row_width, row_start, start, end, temp;

			if (x1 > x2) {
			temp = x2; x2 = x1; x1 = temp;
		}

			if (y1 > y2) {
			temp = y2; y2 = y1; y1 = temp;
		}

			x1 *= 4;
		x2 *= 4;
		row_width = this.width * 4;
		row_start = y1 * row_width;

			for (i = y1; i <= y2; i++) {

				start = x1 + row_start;
			end = x2 + row_start;

				for (j = start; j <= end; j+=4) {
				this.data[j] = color[0];	
				this.data[j + 1] = color[1];	
				this.data[j + 2] = color[2];
				this.data[j + 3] = color[3];	
			}

				row_start += row_width;
		}

		};





						function DepthBuffer(width, height) {
		var size;

			if (height) {
			size = width * height;
		} else {
			size = width;
			height = 1;
		}

			this.width = width;
		this.height = height;

			this.buffer = new Float32Array(size);
	}





	GPU.createColorBuffer = function(w, h, b) {
		return new ColorBuffer(w, h, b);
	};

		GPU.execClearBuffer = function(buffer, color) {
		buffer.clear(color);
	};

		GPU.execRectangle = function(buffer, x1, y1, x2, y2, color) {
		buffer.rect(x1, y1, x2, y2, color);
	};






		function GpuCommandBuffer() {
		this.data = [];
		this.data.length = 4096;

			this.read = 0;
		this.write = 0;
	}

		var proto = GpuCommandBuffer.prototype;

	proto.enqueue = function(item) {

			this.data[this.write] = item;

			this.write++;
		this.write %= this.data.length;
	};

	proto.dequeue = function() {
		var item;

			if (this.read === this.write) {
			return null;	
		}

			item = this.data[this.read];

			this.read++;
		this.read %= this.data.length;

			return item;
	};

	proto.reset = function() {
		this.read = 0;
		this.write = 0;
	};

		proto.process = function() {
		var time, item;

			time = Frame.getTimeLeft(true);

			while (Frame.getTimeLeft() > 0 && (CommandBuffer.read !== CommandBuffer.write)) {
			item = CommandBuffer.dequeue();
			item.func.apply(GPU, item.args);
		}

			Frame.callback({
			frame : Frame.getFrameTime(),
			left : Frame.getTimeLeft(),
			total : (Frame.getFrameTime() - Frame.getTimeLeft())
		});

			this.schedule();
	};

		proto.schedule = function() {

			var This = this;

				if (Frame.running) {
			Frame.getFrame.apply(window, [function() { This.process(); }]);
		}	
	};

		var CommandBuffer = new GpuCommandBuffer();

	GPU.command = function(cmd, args) {
		CommandBuffer.enqueue({func : cmd, args : args});
	};

		GPU.empty = function() {
		CommandBuffer.reset();
	};








			(function(GPU) {

			GPU.Context = function() {

				this.activeVarying = [];
			this.blendEnabled = false;
			this.blendDestA = 0;
			this.blendDestRGB = 0;
			this.blendEquationA = cnvgl.FUNC_ADD;
			this.blendEquationRGB = cnvgl.FUNC_ADD;
			this.blendSrcA = 1;
			this.blendSrcRGB = 1;
			this.clearColor = null;
			this.clearDepth = null;
			this.clearStencil = null;
			this.colorBuffer = null;
			this.colorMask = [0xFF, 0xFF, 0xFF, 0xFF];
			this.cullFlag = false;
			this.cullFrontFace = cnvgl.CCW;
			this.cullFaceMode = cnvgl.BACK;
			this.depthBuffer = null;
			this.depthFunc = cnvgl.LESS;
			this.depthMask = cnvgl.TRUE;
			this.depthTest = null;
			this.mulitsampleCoverageValue = 1;
			this.mulitsampleCoverageInvert = false;
			this.scissorX = 0;
			this.scissorY = 0;
			this.scissorWidth = 0;
			this.scissorHeight = 0;
			this.stencilBuffer = null;
			this.stencilFuncFront = cnvgl.ALWAYS;
			this.stencilFuncBack = cnvgl.ALWAYS;
			this.stencilRefFront = 0;
			this.stencilRefBack = 0;
			this.stencilValueMaskFront = ~0;
			this.stencilValueMaskBack = ~0;
			this.stencilWriteMaskFront = ~0;
			this.stencilWriteMaskBack = ~0;


				this.viewportF = 1;
			this.viewportH = 0;
			this.viewportN = 0;
			this.viewportW = 0;
			this.viewportX = 0;
			this.viewportY = 0;
		};

		}(GPU));



		function Execute(cmd) {

			if (GPU.commands[cmd[1]]) {
			return GPU.commands[cmd[1]].apply(GPU, cmd);
		} else {
		}
	}

		GPU.execute = Execute;

		GPU.commands = {};

		GPU.commands.set = function(ctx, cmd, name, value) {
		ctx[name] = value;
		return true;
	};

		GPU.commands.setArray = function(ctx, cmd, name, index, value) {
		ctx[name][index] = value;
		return true;
	};

		GPU.commands.clear = function(ctx, cmd, mask) {
		if (mask && cnvgl.COLOR_BUFFER_BIT) {
			cnvgl.memseta(ctx.colorBuffer, 0, ctx.clearColor, ctx.colorBuffer.size);
		}
		if (mask && cnvgl.DEPTH_BUFFER_BIT) {
			cnvgl.memset(ctx.depthBuffer, 0, ctx.clearDepth);
		}
		if (mask && cnvgl.STENCIL_BUFFER_BIT) {
			cnvgl.memset(ctx.stencilBuffer, 0, ctx.clearStencil);
		}
		return true;
	};

		var cache = {
		i : -1,
		data : []
	};


			GPU.commands.drawPrimitives = function(ctx, cmd, mode, first, count) {
		var start, now, vertex;

				start = Date.now();
		if (cache.i == -1) {
			cache.i = first;
		}
		for (; cache.i < count; cache.i++) {
			vertex = new Vertex(cache.i);
			GPU.renderer.send(ctx, mode, vertex);

				now = Date.now();
			if (now - start > 200) {
				cache.i++;
				return false;
			}
		}
		GPU.renderer.end(ctx, mode);
		cache.i = -1;
		return true;
	};


			GPU.commands.drawIndexedPrimitives = function(ctx, cmd, mode, indices, first, count, type) {
		var start, now, idx;

				start = Date.now();
		if (cache.i == -1) {
			cache.data = [];
			cache.i = first;
		}

			for (; cache.i < count; cache.i++) {

						idx = indices[first + cache.i];

				if (cache.data[idx]) {
				vertex = cache.data[idx];
			} else {
				vertex = new Vertex(idx);
				cache.data[idx] = vertex;
			}

				GPU.renderer.send(ctx, mode, vertex);

				now = Date.now();
			if (now - start > 200) {
				cache.i++;
				return false;
			}
		}

			GPU.renderer.end(ctx, mode);
		cache.i = -1;
		return true;
	};

			GPU.commands.uploadProgram = function(ctx, cmd, data) {
		GPU.uploadShaders(ctx, data);
		return true;
	};

		GPU.commands.uploadAttributes = function(ctx, cmd, location, size, stride, si, data) {
		var ds, i, c, dest;

			ds = Math.ceil((data.length - si) / (size + stride)) * 4;
		dest = cnvgl.malloc(ds, 1);

			GPU.memory.attributes_src[location] = {
			start : location * 4,
			size : size,
			stride : stride,
			si : si,
			data : dest
		};

				c = 0;
		for (i = 0; i < ds; i++) {

				if (c < size) {
				dest[i] = data[si];
				si++;
			} else {
				dest[i] = (c == 3) ? 1 : 0;
			}

				c++;
			if (c == 4) {
				si += stride;
				c = 0;
			}
		}
		return true;
	};

		GPU.commands.uploadTexture = function(ctx, cmd, unit, texture_obj) {
		GPU.texture.upload(unit, texture_obj);
		return true;
	};

		GPU.commands.uploadUniforms = function(ctx, cmd, location, data, slots, components) {
		var i, j, mem, row, s;

			mem = GPU.memory.uniforms;
		row = 4 * location;
		s = 0;

			for (i = 0; i < slots; i++) {
			for (j = 0; j < components; j++) {
				mem[row + j] = data[s++];
			}
			row += 4;
		}

			return true;
	};




		function GpuFrame() {

			this.start = 0;
		this.running = false;

			this.callback = null;
		this.getFrame = this.getAnimationFrameFunc();

			this.fps = 60;
	}

		var proto = GpuFrame.prototype;

	proto.getAnimationFrameFunc = function() {
		return window.requestAnimationFrame ||
			window.webkitRequestAnimationFrame ||
			window.mozRequestAnimationFrame ||
			window.oRequestAnimationFrame ||
			window.msRequestAnimationFrame ||
			function(func) {
				window.setTimeout(func, 1000 / 60);
			};
	};

		proto.setFps = function(fps) {
		var old;

				old = this.fps;
		this.fps = fps;

				return old;
	};

		proto.getFrameTime = function() {
		return 1000 / this.fps;
	};

		proto.getTimeLeft = function(reset) {
		var total, now;

				total = this.getFrameTime();
		now = Date.now();

			if (reset) {
			this.start = now;
			return total;
		}

			return (total - (now - this.start));
	};


			var Frame = new GpuFrame();



	GPU.setFps = function(fps) {
		return Frame.setFps(fps);
	};

		GPU.getFps = function() {
		return Frame.fps;
	};

		GPU.onFrame = function(func) {
		Frame.callback = func;
	};

		GPU.run = function() {
		Frame.running = true;
		CommandBuffer.schedule();
	};

		GPU.pause = function() {
		Frame.running = false;
	};








			GPU.memory = {};

		GPU.memory.temp = null;
	GPU.memory.uniforms = null;
	GPU.memory.attributes = null;
	GPU.memory.cur_attributes = null;
	GPU.memory.varying = null;
	GPU.memory.result = null;





			function CommandQueue(driver) {
		this.commands = [];
		this.timer = null;
		this.driver = driver;
	}

		proto = CommandQueue.prototype;

	proto.enqueue = function(cmd) {
		this.commands.push(cmd);
		this.schedule();
	};


			proto.process = function() {
		var command, start, now, result;

			this.timer = null;
		start = Date.now();

			while (this.commands.length > 0) {

				command = this.commands.shift();
			result = GPU.execute(command);

						if (!result) {
				this.commands.unshift(command);
				this.schedule();
				return;
			}

				now = Date.now();

				if (this.commands.length > 0 && now - start > 200) {
				this.schedule();
				return;
			}
		}

			this.driver.present();
	};

	proto.schedule = function() {
		var This;
		if (!this.timer) {
			This = this;
			this.timer = setTimeout(function() { This.process(); }, 0);
		}
	};

		GPU.CommandQueue = CommandQueue;






			var shader, result, vertex, temp, program, memory;
	var tex;

		shader = {
		MAX_UNIFORMS : 128,
		MAX_FRAGMENT_UNIFORM_COMPONENTS : 128,
		MAX_VERTEX_ATTRIBS : 16,
		MAX_VARYING_VECTORS : 12,
		MAX_TEMPORARIES : 12
	};

		GPU.executeVertex = function(){};
	GPU.executeFragment = function(){};

		GPU.memory.attributes_src = cnvgl.malloc(shader.MAX_VERTEX_ATTRIBS, 1);

		GPU.uploadShaders = function(state, prgm) {

			state.prgm = prgm;

			this.executeVertex = prgm.vertex;
		this.executeFragment = prgm.fragment;

			GPU.memory.uniforms = prgm.context.uniform_f32;
		GPU.memory.attributes = prgm.context.attribute_f32;
		GPU.memory.varying = prgm.context.varying_f32;
		GPU.memory.result = prgm.context.result_f32;		
	};

		GPU.shader = shader;

	var tex;
	shader.setTexFunc = function(f) { tex = f; };




		var texture, i, j, texUnit;

		texture = {
		MAX_TEXTURE_COORDS : 4,
		MAX_COMBINED_TEXTURE_IMAGE_UNITS : 2
	};

		texUnit = [];
	for (i = 0; i < texture.MAX_COMBINED_TEXTURE_IMAGE_UNITS; i++) {
		texUnit[i] = null;
	}

		function tex(c, ci, src, si, sampler, target) {
		var texture, mipmap_level, img, img_w, img_h, img_d, i, u, v, a, b, i1, s, t;

				s = src[si];
		t = src[si + 1];

			target = 0;
		mipmap_level = 0;

			texture = texUnit[sampler];

				if (!texture) {
			c[ci + 0] = 0;
			c[ci + 1] = 0;
			c[ci + 2] = 0;
			c[ci + 3] = 1;
			return;
		}

				img = texture.images[mipmap_level];

			if (!img) {
			c[ci + 0] = 0;
			c[ci + 1] = 0;
			c[ci + 2] = 0;
			c[ci + 3] = 1;
			return;
		}

			img_w = img.width;
		img_h = img.height;
		img_d = img.data;

			switch (texture.min_filter) {
			case cnvgl.LINEAR:

					var ui, vi, u0v0, u1v0, u0v1, u1v1, ai, bi;

					u = (s * (img_w - 1));
				v = (t * (img_h - 1));
				ui = (u | 0); 
				vi = (v | 0); 
				a = u - ui;
				b = v - vi;

					u0v0 = (1 - a) * (1 - b);
				u1v0 =      a  * (1 - b);
				u0v1 = (1 - a) *      b ;
				u1v1 =      a  *      b ;

					i = (vi * img_w + ui) * 4;
				i1 = i + (img_w * 4);

					c[ci + 0] = u0v0 * img_d[i    ] + u1v0 * img_d[i + 4] + u0v1 * img_d[i1    ] + u1v1 * img_d[i1 + 4];
				c[ci + 1] = u0v0 * img_d[i + 1] + u1v0 * img_d[i + 5] + u0v1 * img_d[i1 + 1] + u1v1 * img_d[i1 + 5];
				c[ci + 2] = u0v0 * img_d[i + 2] + u1v0 * img_d[i + 6] + u0v1 * img_d[i1 + 2] + u1v1 * img_d[i1 + 6];
				c[ci + 3] = u0v0 * img_d[i + 3] + u1v0 * img_d[i + 7] + u0v1 * img_d[i1 + 3] + u1v1 * img_d[i1 + 7];

								break;

				case cnvgl.NEAREST:
			default:
				u = (s * img_w)|0; 
				v = (t * img_h)|0; 
				if (u == img_w) {
					u--;
				}
				if (v == img_h) {
					v--;
				}
				i = (v * img_w + u) * 4;
				c[ci + 0] = img_d[i];
				c[ci + 1] = img_d[i + 1];
				c[ci + 2] = img_d[i + 2];
				c[ci + 3] = img_d[i + 3];
		}

		}

		GPU.texture = texture;

		GPU.tex = tex;

		GPU.texture.upload = function(unit, texture_obj) {
		texUnit[unit] = texture_obj;
	};




	function cnvgl_rendering_clipping(renderer) {

			this.ctx = null;
		this.renderer = renderer;

			this.v1 = null;
		this.v2 = null;
		this.v3 = null;

			this.planes = [[ 1,  0,  0],
		               [-1,  0,  0],
		               [ 0,  1,  0],
		               [ 0, -1,  0],
		               [ 0,  0,  1],
		               [ 0,  0, -1]];
	}

		proto = cnvgl_rendering_clipping.prototype;

	proto.clipPoint = function(prim) {
		var p;

			p = prim.vertices[0];

			if (p.xd < -1 || p.xd > 1 ||
			p.yd < -1 || p.yd > 1 ||
			p.zd < -1 || p.zd > 1) {
			return 0;
		}

			return 1;
	};

	proto.clipLine = function(prim, clipped) {
		clipped.push(prim);
		return 1;
	};

	proto.clipTriangle = function(state, prim, clipped) {
		var i, p, nprim;

		this.v1 = prim.vertices[0];
		this.v2 = prim.vertices[1];
		this.v3 = prim.vertices[2];

			this.renderer.interpolate.setVertices(this.v1, this.v2, this.v3);

			for (i = 0; i < this.planes.length; i++) {
			p = this.planes[i];
			if (!this.clipTriangleToPlane(state, prim, p[0], p[1], p[2], 1)) {
				return 0;
			}
		}


				for (i = 0; i < prim.vertices.length; i+=3) {
			nprim = new Primitive();
			nprim.vertices.push(prim.vertices[i]);
			nprim.vertices.push(prim.vertices[i + 1]);
			nprim.vertices.push(prim.vertices[i + 2]);
			clipped.push(nprim);
		}

			return clipped.length;
	};

	proto.interpolate = function(state, v1, v2, amt) {
		var int, xw, yw, vr, namt, v;

			int = this.renderer.interpolate;

			namt = 1 - amt;

			xw = v1.xw * namt + v2.xw * amt;
		yw = v1.yw * namt + v2.yw * amt;

			int.setPoint(xw, yw);

			vr = new Vertex();
		vr.varying = new Float32Array(v1.varying);
		vr.result = new Float32Array(v1.result);

		vr.xw = v1.xw * namt + v2.xw * amt;
		vr.yw = v1.yw * namt + v2.yw * amt;
		vr.zw = v1.zw * namt + v2.zw * amt;

			vr.w = v1.w * namt + v2.w * amt;

		int.interpolateVarying(state, this.v1, this.v2, this.v3, vr.varying);

			return vr;
	};

	proto.clipTriangleToPlane = function(state, prim, px, py, pz, pd) {
		var v1, v2, v3, d1, d2, d3, cx, n, l;

			n = 0;
		l = prim.vertices.length;

			while (n < l) {

				v1 = prim.vertices[n];
			v2 = prim.vertices[n + 1];
			v3 = prim.vertices[n + 2];

				d1 = (v1.xd * px + v1.yd * py + v1.zd * pz);
			d2 = (v2.xd * px + v2.yd * py + v2.zd * pz);
			d3 = (v3.xd * px + v3.yd * py + v3.zd * pz);

				cx = (d1 <= pd ? 1 : 0) + (d2 <= pd ? 1 : 0) + (d3 <= pd ? 1 : 0);

				if (cx == 0) {

					if (n == 0 && prim.vertices.length == 3) {
					return false;
				}

					prim.vertices.splice(n, 3); 
				l -= 3; 
				n -= 3; 

				} else if (cx == 1) { 

					if (d1 <= pd) { 
					prim.vertices[n + 1] = this.interpolate(state, v1, v2, (pd -d1)/(d2-d1));
					prim.vertices[n + 2] = this.interpolate(state, v1, v3, (pd-d1)/(d3-d1));
				} else if (d2 <= pd) { 
					prim.vertices[n + 0] = this.interpolate(state, v2, v1, (pd-d2)/(d1-d2));
					prim.vertices[n + 2] = this.interpolate(state, v2, v3, (pd-d2)/(d3-d2));
				} else { 
					prim.vertices[n + 0] = this.interpolate(state, v3, v1, (pd-d3)/(d1-d3));
					prim.vertices[n + 1] = this.interpolate(state, v3, v2, (pd-d3)/(d2-d3));
				}

				} else if (cx == 2) { 
				if (d1 > pd) { 
					prim.vertices[n + 0] = this.interpolate(state, v2, v1, (pd-d2)/(d1-d2));
					prim.vertices.push(prim.vertices[n], prim.vertices[n + 2], this.interpolate(state, v3, v1, (pd-d3)/(d1-d3)));
				} else if (d2 > pd) { 
					prim.vertices[n + 1] = this.interpolate(state, v3, v2, (pd-d3)/(d2-d3));
					prim.vertices.push(prim.vertices[n], this.interpolate(state, v1, v2, (pd-d1)/(d2-d1)), prim.vertices[n + 1]);
				} else { 
					prim.vertices[n + 2] = this.interpolate(state, v1, v3, (pd-d1)/(d3-d1));
					prim.vertices.push(this.interpolate(state, v2, v3, (pd-d2)/(d3-d2)), prim.vertices[n + 2], prim.vertices[n + 1]);
				}
			} 

				n += 3;
		}

			return prim.vertices.length > 0;
	};




	function cnvgl_rendering_culling(renderer) {
		this.renderer = renderer;
	}

		proto = cnvgl_rendering_culling.prototype;

	proto.checkCull = function(state, prim) {
		var dir;
		if (state.cullFlag) {

			if (state.cullFaceMode == cnvgl.FRONT_AND_BACK) {
				return true;	
			}

				dir = this.getPolygonFaceDir(prim);
			if (!(
				(dir > 0 && (state.cullFlag == cnvgl.FALSE || state.cullFaceMode == cnvgl.FRONT)) ||
				(dir < 0 && (state.cullFlag == cnvgl.FALSE || state.cullFaceMode == cnvgl.BACK)))) {
				return true;
			}
		}
		return false;
	};

	proto.getPolygonFaceDir = function(state, prim) {
		var dir;
		dir = prim.getDirection();
		if (state.cullFrontFace == cnvgl.CCW) {
			dir = -dir;
		}
		return dir;
	};



	function cnvgl_rendering_fragment(renderer) {
			this.renderer = renderer;
	}

		var proto = cnvgl_rendering_fragment.prototype;

	proto.loadAttributes = function(state, f) {
		var attr, i, j;

			for (i = 0; i < state.activeVarying.length; i++) {
			attr = state.activeVarying[i];

				if (attr) {
				for (j = 0; j < attr; j++) {
					GPU.memory.varying[4 * i + j] = f.attrib[4 * i + j];	
				}
			}
		}
	};

	proto.process = function(state, f) {
		var i;

			this.loadAttributes(state, f);

				GPU.executeFragment();
	};

	proto.write = function(state, i, frag) {
		var c_buffer, c, result, c_mask;

			if (state.depthMask) {
			state.depthBuffer[i] = frag.gl_FragDepth;
		}

			i <<= 2;

			result = GPU.memory.result;
		c = frag.color;
		c[0] = result[0] * 255;
		c[1] = result[1] * 255;
		c[2] = result[2] * 255;
		c[3] = result[3] * 255;

			c_buffer = state.colorBuffer.data;

			if (state.blendEnabled) {
			this.blend(state, c, c[0], c[1], c[2], c[3], c_buffer[i], c_buffer[i + 1], c_buffer[i + 2], c_buffer[i + 3]);
		}

			c_mask = state.colorMask;
		c_buffer[i    ] = c_mask[0] & (c[0] + .5)|0; 
		c_buffer[i + 1] = c_mask[1] & (c[1] + .5)|0; 
		c_buffer[i + 2] = c_mask[2] & (c[2] + .5)|0; 
		c_buffer[i + 3] = c_mask[3] & (c[3] + .5)|0; 
	};

	proto.blend = function(state, color, sr, sg, sb, sa, dr, dg, db, da) {
		var state, a_sr, a_sg, a_sb, a_sa, a_dr, a_dg, a_db, a_da;

				switch (state.blendSrcA) {
			case cnvgl.ONE:
				a_sr = a_sg = a_sb = a_sa = (1);
				break;
			case cnvgl.ZERO:
				a_sr = a_sg = a_sb = a_sa = (0);
				break;
			case cnvgl.SRC_ALPHA:
				a_sr = a_sg = a_sb = a_sa = (sa / 255);
				break;
			case cnvgl.ONE_MINUS_SRC_ALPHA:
				a_sr = a_sg = a_sb = a_sa = (1 - (sa / 255));
				break;
			case cnvgl.DST_ALPHA:
				a_sr = a_sg = a_sb = a_sa = (da / 255);
				break;
			case cnvgl.ONE_MINUS_DST_ALPHA:
				a_sr = a_sg = a_sb = a_sa = (1 - (da / 255));
				break;
			default:
				throw new Error('Blend source ' + state.blendSrcA + ' not implemented');
		}

			switch (state.blendDestA) {
			case cnvgl.ONE:
				a_dr = a_dg = a_db = a_da = (1);
				break;
			case cnvgl.ZERO:
				a_dr = a_dg = a_db = a_da = (0);
				break;
			case cnvgl.SRC_ALPHA:
				a_dr = a_dg = a_db = a_da = (sa / 255);
				break;
			case cnvgl.ONE_MINUS_SRC_ALPHA:
				a_dr = a_dg = a_db = a_da = (1 - (sa / 255));
				break;
			case cnvgl.DST_ALPHA:
				a_dr = a_dg = a_db = a_da = (da / 255);
				break;
			case cnvgl.ONE_MINUS_DST_ALPHA:
				a_dr = a_dg = a_db = a_da = (1 - (da / 255));
				break;
			default:
				throw new Error('Blend source ' + state.blendSrcD + ' not implemented');					
		}

			switch (state.blendEquationRGB) {
			case cnvgl.FUNC_ADD:
				color[0] = (a_sr * sr) + (a_dr * dr);
				color[1] = (a_sg * sg) + (a_dg * dg);
				color[2] = (a_sb * sb) + (a_db * db);
				break;
			default:
				throw new Error('Blend function ' + state.blendEquationRGB + ' not implemented');									
		}

			switch (state.blendEquationA) {
			case cnvgl.FUNC_ADD:
				color[3] = (a_sa * sa) + (a_da * da);
				break;
			default:
				throw new Error('Blend function ' + state.blendEquationRGB + ' not implemented');									
		}

				if (color[0] > 255) { color[0] = 255; }
		if (color[1] > 255) { color[1] = 255; }
		if (color[2] > 255) { color[2] = 255; }
		if (color[3] > 255) { color[3] = 255; }

			};



	function cnvgl_rendering_interpolate(renderer) {

			this.ctx = null;
		this.renderer = renderer;

				this.v1 = null;
		this.v2 = null;
		this.v3 = null;

			this.a = null;
		this.b = null;
		this.c = null;
		this.wa = null;
		this.wb = null;
		this.wc = null;
		this.t = {};

			this.attributes = null;
		this.varying = null;
	}

		proto = cnvgl_rendering_interpolate.prototype;


	proto.setVertices = function(v1, v2, v3) {


			this.v1 = [v1.xw, v1.yw, v1.zw, v1.w];
		this.v2 = [v2.xw, v2.yw, v2.zw, v2.w];
		if (v3) {
			this.v3 = [v3.xw, v3.yw, v3.zw, v3.w];
		} else {
			this.v3 = null;	
		}
		this.precompute();
	};


	proto.interpolateVarying = function(state, v1, v2, v3, dest) {
		var i;
		for (i = 0; i < state.activeVarying.length; i++) {
			if (state.activeVarying[i]) {
				this.interpolateTriangleVector(v1.varying, v2.varying, v3.varying, dest, state.activeVarying[i], i * 4);
			}
		}
	};

	proto.precompute = function() {
		var x1, x2, x3, y1, y2, y3, t;

			x1 = this.v1[0];
		x2 = this.v2[0];
		y1 = this.v1[1];
		y2 = this.v2[1];

			t = {};

			if (this.v3) {
			x3 = this.v3[0];		
			y3 = this.v3[1];

				t.a = (x2 - x1);
			t.b = (x3 - x1);
			t.c = (y2 - y1);
			t.d = (y3 - y1);
			t.e = (t.c / t.a);
			t.f = (t.d + t.e * t.b);
			t.g = 1 / (t.a * t.d - t.b * t.c);  

				this.wa = 1 / this.v1[3];
			this.wb = 1 / this.v2[3];
			this.wc = 1 / this.v3[3];

				} else {
			t.a = (x2 - x1);
			t.b = (y2 - y1);
			t.c = Math.sqrt(t.a * t.a + t.b * t.b);
		}

			this.t = t;
	};

	proto.setPoint = function(x, y) {
		var  x1, y1;

			x1 = this.v1[0];
		y1 = this.v1[1];

			if (this.v3) {

				this.b = (this.t.b * (y1 - y) + this.t.d * (x - x1)) * this.t.g;
			this.c = (this.t.a * (y - y1) - this.t.c * (x - x1)) * this.t.g;
			this.a = 1 - this.b - this.c;

				this.a *= this.wa;
			this.b *= this.wb;
			this.c *= this.wc;

				this.t.p = 1 / (this.a + this.b + this.c);

				} else {

				x = (x - x1);
			y = (y - y1);
			this.a = Math.sqrt(x * x + y * y);
			this.a = this.a / this.t.c;
			this.b = 1 - this.a;
		}
	};

	proto.interpolateLine = function(f1, f2) {
		var i, v;

		if (typeof f1 == 'object') {
			v = [];
			for (i = 0; i < f1.length; i++) {
				v[i] = ((this.a * f1[i]) + (this.b * f2[i])) ;
			}
		} else {
			v = ((this.a * f1) + (this.b * f2)) ;
		}
		return v;				
	};

	proto.interpolateTriangle = function(f1, f2, f3) {
		var v;
		v = ((this.a * f1) + (this.b * f2) + (this.c * f3)) * this.t.p;
		return v;
	};

	proto.interpolateTriangleVector = function(f1, f2, f3, dest, size, start) {
		var i;
		for (i = 0; i < size; i++) {
			dest[start + i] = ((this.a * f1[start + i]) + (this.b * f2[start + i]) + (this.c * f3[start + i])) * this.t.p;
		}
	};



	function cnvgl_rendering_primitive(renderer) {

			this.renderer = renderer;

			this.line = new cnvgl_rendering_primitive_line(renderer);
		this.point = new cnvgl_rendering_primitive_point(renderer);
		this.triangle = new cnvgl_rendering_primitive_triangle(renderer);

				this.vertices = [];
	}

		proto = cnvgl_rendering_primitive.prototype;

	proto.send = function(state, mode, vertex) {
		this.vertices.push(vertex);
		switch (mode) {
			case cnvgl.POINTS:
				this.points(state);
				break;
			case cnvgl.LINES:
				this.lines(state);
				break;
			case cnvgl.LINE_STRIP:
				this.lineStrip(state);
				break;
			case cnvgl.LINE_LOOP:
				this.lineLoop(state);
				break;
			case cnvgl.TRIANGLES:
				this.triangles(state);
				break;
			case cnvgl.TRIANGLE_STRIP:
				this.triangleStrip(state);
				break;
		}
	};

	proto.end = function(state, mode) {
		switch (mode) {
			case cnvgl.LINE_LOOP:
				this.vertices.push(this.vertices.shift());
				this.lines(state);
				break;
		}
		this.vertices = [];
	};

	proto.points = function(state) {
		var prim;
		prim = new Primitive();
		prim.vertices.push(this.vertices.shift());
		this.point.render(state, prim);
	};

	proto.lines = function(state) {
		var prim;
		if (this.vertices.length > 1) {
			prim = new Primitive();
			prim.vertices.push(this.vertices.shift());
			prim.vertices.push(this.vertices.shift());
			this.line.render(state, prim);
		}
	};

	proto.lineStrip = function(state) {
		var prim;
		if (this.vertices.length > 1) {
			prim = new Primitive();
			prim.vertices.push(this.vertices.shift());
			prim.vertices.push(this.vertices[0]);
			this.line.render(state, prim);
		}
	};

	proto.lineLoop = function(state) {
		var prim, v0;
		if (this.vertices.length < 2) {
			return;
		}
		prim = new Primitive();
		if (this.vertices.length > 2) {
			v0 = this.vertices.shift();
			prim.vertices.push(this.vertices.shift());
			prim.vertices.push(this.vertices[0]);
			this.vertices.unshift(v0);
		} else {
			prim.vertices.push(this.vertices[0]);
			prim.vertices.push(this.vertices[1]);
		}
		this.line.render(state, prim);
	};

	proto.triangles = function(state) {
		var prim;
		if (this.vertices.length > 2) {
			prim = new Primitive();
			prim.vertices.push(this.vertices.shift());	
			prim.vertices.push(this.vertices.shift());	
			prim.vertices.push(this.vertices.shift());
			this.triangle.render(state, prim);
		}
	};

	proto.triangleStrip = function(state) {
		var prim;
		if (this.vertices.length > 2) {
			prim = new Primitive();
			prim.vertices.push(this.vertices.shift());	
			prim.vertices.push(this.vertices[0]);
			prim.vertices.push(this.vertices[1]);
			this.triangle.render(state, prim);
		}
	};



	function cnvgl_renderer() {
		this.clipping = new cnvgl_rendering_clipping(this);
		this.culling = new cnvgl_rendering_culling(this);
		this.interpolate = new cnvgl_rendering_interpolate(this);
		this.primitive = new cnvgl_rendering_primitive(this);
		this.fragment = new cnvgl_rendering_fragment(this);
		this.vertex = new cnvgl_rendering_vertex(this);
	}

		proto = cnvgl_renderer.prototype;

	proto.send = function(state, mode, vertex) {
		if (!vertex.processed) {
			this.vertex.process(state, vertex);
		}
		this.primitive.send(state, mode, vertex);
	};

	proto.end = function(mode) {
		this.primitive.end(mode);
	};

	proto.checkDepth = function(state, i, z) {
		var depth, pass;

			depth = state.depthBuffer[i];

			switch (state.depthFunc) {
			case cnvgl.NEVER:
				pass = false;
				break;
			case cnvgl.ALWAYS:
				pass = true;
				break;
			case cnvgl.LESS:
				pass = z < depth;
				break;
			case cnvgl.LEQUAL:
				pass = z <= depth;
				break;
			case cnvgl.EQUAL:
				pass = z == depth;
				break;
			case cnvgl.GREATER:
				pass = z > depth;
				break;
			case cnvgl.GEQUAL:
				pass = z >= depth;
				break;
			case cnvgl.NOTEQUAL:
				pass = z != depth;
				break;
			default:
				pass = true;
		}		
		return pass;
	};




	function cnvgl_rendering_vertex(renderer) {
		this.renderer = renderer;
	}

		proto = cnvgl_rendering_vertex.prototype;

	proto.loadAttributes = function(state, n) {
		var src, attr, i, j;

				src = GPU.memory.attributes_src;

			for (i = 0; i < src.length; i++) {

				attr = src[i];

						if (!attr) {
				break;
			}

				for (j = 0; j < attr.size; j++) {
				GPU.memory.attributes[attr.start + j] = attr.data[n * 4 + j];
			}
		}

			};

	proto.process = function(state, v) {

			this.loadAttributes(state, v.i);

			GPU.executeVertex();

			v.varying = new Float32Array(GPU.memory.varying);
		v.result = new Float32Array(GPU.memory.result);

			v.x = v.result[0];
		v.y = v.result[1];
		v.z = v.result[2];
		v.w = v.result[3];

		if (v.w) {
			v.xd = v.x / v.w;
			v.yd = v.y / v.w;
			v.zd = v.z / v.w;

			v.xw = state.viewportX + (state.viewportW / 2) * (1 + v.xd);
			v.yw = state.viewportY + (state.viewportH / 2) * (1 - v.yd);
			v.zw = (((state.viewportF - state.viewportN) * v.zd) + state.viewportN + state.viewportF) / 2;
		}
	};

	proto.sortVertices = function(prim) {

			if (prim.sorted) {
			return;
		}

			var ymin = 99999, yminx = 9999, yi, i, vs, vertices= [];
		vs = prim.vertices;

		if (vs.length < 2) {
			return;
		}

		for (i = 0; i < vs.length; i++) {
			if (vs[i].yw < ymin || (vs[i].yw == ymin && vs[i].xw < yminx)) {
				ymin = vs[i].yw;
				yminx = vs[i].xw;
				yi = i;
			}
		}

		for (i = 0; i < vs.length; i++) {
			vertices[i] = vs[yi];
			yi++;
			if (yi >= vs.length) {
				yi = 0;
			}
		}

			prim.vertices = vertices;
		prim.sorted = true;
	};

	proto.slope = function(x1, y1, x2, y2) {
		x1 = x2 - x1;
		y1 = y2 - y1;
		return (x1 / y1);
	};





	function Fragment() {
		this.attrib = null;
		this.result = null;
		this.color = new Float32Array(4);
	};



	function Primitive() {
		this.mode = null;
		this.vertices = [];
		this.sorted = false;
		this.direction = null;
	}

		proto = Primitive.prototype;

	proto.getDirection = function() {
		var a, E, i, th, n;

			if (this.direction) {
			return this.direction;	
		}

			n = this.vertices.length;
		E = 0;
		for (i = 0; i < n; i++) {
			th = (i + 1) % n;
			E += (this.vertices[i].xw * this.vertices[th].yw - this.vertices[th].xw * this.vertices[i].yw);
		}
		E = E > 0 ? 1 : -1;

				this.direction = E;

			return this.direction;
	};



	function Vertex(i) {

			this.processed = false;

				this.x = 0;
		this.y = 0;
		this.z = 0;
		this.w = 0;

				this.xd = 0;
		this.yd = 0;
		this.zd = 0;

			this.xw = 0;
		this.yw = 0;
		this.zw = 0;

				this.xc = 0;
		this.yc = 0;
		this.zc = 0;

				this.i = i;

		this.varying = null;
		this.result = null;
	};



	function cnvgl_rendering_primitive_line(renderer) {
		this.renderer = renderer;

				this.prim = null;
		this.frag = new Fragment();
	}

		proto = cnvgl_rendering_primitive_line.prototype;

	proto.render = function(state, prim) {
		var clipped, num, i;

			clipped = [];
		num = this.renderer.clipping.clipLine(prim, clipped);

			for (i = 0; i < num; i++) {
			this.renderClipped(state, clipped[i]);
		}
	};

	proto.renderClipped = function(state, prim) {
		var v1, v2, dx, dy, dir;
		this.prim = prim;
		v1 = prim.vertices[0];
		v2 = prim.vertices[1];

			dx = this.renderer.vertex.slope(v1.xw, v1.yw, v2.xw, v2.yw);
		dy = this.renderer.vertex.slope(v1.yw, v1.xw, v2.yw, v2.xw);		
		dir = Math.abs(dx) > Math.abs(dy) ? 1 : -1; 

			if (dir > 0) {
			this.lineX(state, v1, v2, dy);
		} else {
			this.lineY(state, v1, v2, dx);
		}
	};

	proto.lineX = function(state, v1, v2, dy) {
		var frag, x_start, x_end, xi_start, xi_end, y, v, xi, yi, i;

		if (v2.xw < v1.xw) {
			v = v2; v2 = v1; v1 = v;
		}

			this.renderer.interpolate.setVertices(v2, v1);

			x_start = v1.xw;
		x_end = v2.xw;
		xi_start = Math.ceil(x_start);
		xi_end = Math.floor(x_end);
		y = v1.yw + (xi_start - v1.xw) * dy;

			for (xi = xi_start; xi <= xi_end; xi++) {

				yi = (y|0); 
			this.renderer.interpolate.setPoint(xi, yi);

				i = (state.viewportW * yi + xi);

				this.renderer.fragment.process(state, this.frag);
			this.renderer.fragment.write(state, i, this.frag);

				y += dy;
		}
	};

	proto.lineY = function(state, v1, v2, dx) {
		var frag, y_start, y_end, yi_start, yi_end, x, v, yi, xi, i;

		if (v2.yw < v1.yw) {
			v = v2; v2 = v1; v1 = v;
		}

			this.renderer.interpolate.setVertices(v2, v1);

			y_start = v1.yw;
		y_end = v2.yw;
		yi_start = Math.ceil(y_start);
		yi_end = (y_end)|0; 
		x = v1.xw + (yi_start - v1.yw) * dx;

			for (yi = yi_start; yi <= yi_end; yi++) {

				xi = (x|0); 
			this.renderer.interpolate.setPoint(xi, yi);


				i = (state.viewportW * yi + xi);

				this.renderer.fragment.process(state, this.frag);
			this.renderer.fragment.write(state, i, this.frag);

				x += dx;
		}
	};



	function cnvgl_rendering_primitive_point(renderer) {

			this.renderer = renderer;
		this.frag = new Fragment();

			this.prim = null;
	}

		proto = cnvgl_rendering_primitive_point.prototype;

	proto.render = function(state, prim) {
		var num;

			num = this.renderer.clipping.clipPoint(prim);

			if (num) {
			this.renderClipped(state, prim);
		}

		};

	proto.renderClipped = function(state, prim) {
		var vw, v, x, y, i;

			this.prim = prim;

			v = prim.vertices[0];
		x = Math.round(v.xw);
		y = Math.round(v.yw);

			vw = state.viewportW;


			i = (vw * y + x);

			this.renderer.fragment.process(state, this.frag);
		this.renderer.fragment.write(state, i, this.frag);
	};




	function cnvgl_rendering_primitive_triangle(renderer) {

			this.renderer = renderer;
		this.frag = new Fragment();

			this.prim = null;
		this.v1 = null;
		this.v2 = null;
		this.v3 = null;
	}

		proto = cnvgl_rendering_primitive_triangle.prototype;

	proto.render = function(state, prim) {
		var clipped, num, i;

			if (this.renderer.culling.checkCull(state, prim)) {
			return;
		}

		clipped = [];
		num = this.renderer.clipping.clipTriangle(state, prim, clipped);

			for (i = 0; i < num; i++) {
			this.renderClipped(state, clipped[i]);
		}
	};

	proto.renderClipped = function(state, prim) {
		var dir, t;

			this.prim = prim;

		this.renderer.vertex.sortVertices(prim);
		dir = prim.getDirection();

			if (dir >= 0) {
			t = prim.vertices[2];
			prim.vertices[2] = prim.vertices[1];
			prim.vertices[1] = t;
		}

			this.rasterize(state, prim);
	};

	proto.rasterize = function(state, prim) {
		var v1, v2, v3, dx1, dx2, dx3, yi_start, yi_end, yi, x_start, x_end, vpass;

			v1 = this.v1 = prim.vertices[0];
		v2 = this.v2 = prim.vertices[1];
		v3 = this.v3 = prim.vertices[2];

			this.renderer.interpolate.setVertices(this.v1, this.v2, this.v3);

			dx1 = this.renderer.vertex.slope(v1.xw, v1.yw, v2.xw, v2.yw);
		dx2 = this.renderer.vertex.slope(v1.xw, v1.yw, v3.xw, v3.yw);
		dx3 = this.renderer.vertex.slope(v2.xw, v2.yw, v3.xw, v3.yw);

		yi_start = (v1.yw|0) + .5; 
		if (yi_start < v1.yw) {
			yi_start++;
		}
		yi = v3.yw > v2.yw ? v3.yw : v2.yw;
		yi_end = yi + 1;
		if (yi_end >= yi) {
			yi_end--;
		}

			x_start = v1.xw + (yi_start - v1.yw) * dx1;
		x_end = v1.xw + (yi_start - v1.yw) * dx2;
		vpass = false;

		for (yi = yi_start; yi < yi_end; yi++) {

			if (!vpass && yi > v2.yw) {
				x_start = v3.xw + (yi - v3.yw) * dx3;
				dx1 = dx3;
				vpass = true;
			}

			if (!vpass && yi > v3.yw) {
				x_end = v3.xw + (yi - v3.yw) * dx3;
				dx2 = dx3;
				vpass = true;
			}

				this.rasterizeScanline(state, yi, x_start, x_end);

				x_start += dx1;
			x_end += dx2;
		}
	};

	proto.rasterizeScanline = function(state, yi, x_start, x_end) {
		var int, xi_start, xi_end, xi, i, v;

			int = this.renderer.interpolate;

		xi_start = (x_start|0) + .5; 
		if (xi_start < x_start) {
			xi_start++;	
		}
		xi_end = ((x_end + 1-1e-10)|0) - .5;
		if (xi_end >= x_end) {
			xi_end--;
		}

			i = state.viewportW * (yi - .5) + (xi_start - .5);

			for (xi = xi_start; xi <= xi_end; xi++) {

				int.setPoint(xi, yi);

			if (state.depthTest) {
				this.frag.gl_FragDepth = int.interpolateTriangle(this.v1.zw, this.v2.zw, this.v3.zw);
				if (!this.renderer.checkDepth(state, i, this.frag.gl_FragDepth)) {
					i++;
					continue;
				}
			}

				if (!this.frag.attrib) {
				this.frag.attrib = new Float32Array(this.v1.varying);
				this.frag.result = new Float32Array(this.v1.result);				
			}
			int.interpolateVarying(state, this.v1, this.v2, this.v3, this.frag.attrib);

				this.renderer.fragment.process(state, this.frag);
			this.renderer.fragment.write(state, i, this.frag);

				i++;
		}		
	};




			GPU.initialize();

	this.GPU = GPU;

}());




cnvgl = {

	currentContext : null,

	createContext : function(driver) {
		var ctx;
		ctx = new cnvgl.context(driver);
		return ctx;
	},

	setContext : function(context) {
		cnvgl.currentContext = context;
	},

	getCurrentContext : function() {
		return cnvgl.currentContext;
	},

	throw_error : function(error, ctx) {
		ctx = ctx || cnvgl.getCurrentContext();
		if (error && ctx.errorValue == cnvgl.NO_ERROR) {
			ctx.errorValue = error;
		}
	}

};




cnvgl.ES_VERSION_2_0                 = 1;

cnvgl.DEPTH_BUFFER_BIT               = 0x00000100;
cnvgl.ACCUM_BUFFER_BIT               = 0x00000200;
cnvgl.STENCIL_BUFFER_BIT             = 0x00000400;
cnvgl.COLOR_BUFFER_BIT               = 0x00004000;

cnvgl.FALSE                          = 0;
cnvgl.TRUE                           = 1;

cnvgl.POINTS                         = 0x0000;
cnvgl.LINES                          = 0x0001;
cnvgl.LINE_LOOP                      = 0x0002;
cnvgl.LINE_STRIP                     = 0x0003;
cnvgl.TRIANGLES                      = 0x0004;
cnvgl.TRIANGLE_STRIP                 = 0x0005;
cnvgl.TRIANGLE_FAN                   = 0x0006;


cnvgl.ZERO                           = 0;
cnvgl.ONE                            = 1;
cnvgl.SRC_COLOR                      = 0x0300;
cnvgl.ONE_MINUS_SRC_COLOR            = 0x0301;
cnvgl.SRC_ALPHA                      = 0x0302;
cnvgl.ONE_MINUS_SRC_ALPHA            = 0x0303;
cnvgl.DST_ALPHA                      = 0x0304;
cnvgl.ONE_MINUS_DST_ALPHA            = 0x0305;

cnvgl.DST_COLOR                      = 0x0306;
cnvgl.ONE_MINUS_DST_COLOR            = 0x0307;
cnvgl.SRC_ALPHA_SATURATE             = 0x0308;

cnvgl.FUNC_ADD                       = 0x8006;
cnvgl.BLEND_EQUATION                 = 0x8009;
cnvgl.BLEND_EQUATION_RGB             = 0x8009;    
cnvgl.BLEND_EQUATION_ALPHA           = 0x883D;

cnvgl.FUNC_SUBTRACT                  = 0x800A;
cnvgl.FUNC_REVERSE_SUBTRACT          = 0x800B;

cnvgl.BLEND_DST_RGB                  = 0x80C8;
cnvgl.BLEND_SRC_RGB                  = 0x80C9;
cnvgl.BLEND_DST_ALPHA                = 0x80CA;
cnvgl.BLEND_SRC_ALPHA                = 0x80CB;
cnvgl.CONSTANT_COLOR                 = 0x8001;
cnvgl.ONE_MINUS_CONSTANT_COLOR       = 0x8002;
cnvgl.CONSTANT_ALPHA                 = 0x8003;
cnvgl.ONE_MINUS_CONSTANT_ALPHA       = 0x8004;
cnvgl.BLEND_COLOR                    = 0x8005;

cnvgl.ARRAY_BUFFER                   = 0x8892;
cnvgl.ELEMENT_ARRAY_BUFFER           = 0x8893;
cnvgl.ARRAY_BUFFER_BINDING           = 0x8894;
cnvgl.ELEMENT_ARRAY_BUFFER_BINDING   = 0x8895;

cnvgl.STREAM_DRAW                    = 0x88E0;
cnvgl.STREAM_READ                    = 0x88E1;
cnvgl.STREAM_COPY                    = 0x88E2;
cnvgl.STATIC_DRAW                    = 0x88E4;
cnvgl.STATIC_READ                    = 0x88E5;
cnvgl.STATIC_COPY                    = 0x88E6;
cnvgl.DYNAMIC_DRAW                   = 0x88E8;
cnvgl.DYNAMIC_READ                   = 0x88E9;
cnvgl.DYNAMIC_COPY                   = 0x88EA;

cnvgl.BUFFER_SIZE                    = 0x8764;
cnvgl.BUFFER_USAGE                   = 0x8765;

cnvgl.CURRENT_VERTEX_ATTRIB          = 0x8626;

cnvgl.FRONT                          = 0x0404;
cnvgl.BACK                           = 0x0405;
cnvgl.FRONT_AND_BACK                 = 0x0408;


cnvgl.TEXTURE_2D                     = 0x0DE1;
cnvgl.CULL_FACE                      = 0x0B44;
cnvgl.BLEND                          = 0x0BE2;
cnvgl.DITHER                         = 0x0BD0;
cnvgl.STENCIL_TEST                   = 0x0B90;
cnvgl.DEPTH_TEST                     = 0x0B71;
cnvgl.SCISSOR_TEST                   = 0x0C11;
cnvgl.POLYGON_OFFSET_FILL            = 0x8037;
cnvgl.SAMPLE_ALPHA_TO_COVERAGE       = 0x809E;
cnvgl.SAMPLE_COVERAGE                = 0x80A0;

cnvgl.NO_ERROR                       = 0;
cnvgl.INVALID_ENUM                   = 0x0500;
cnvgl.INVALID_VALUE                  = 0x0501;
cnvgl.INVALID_OPERATION              = 0x0502;
cnvgl.OUT_OF_MEMORY                  = 0x0505;

cnvgl.CW                             = 0x0900;
cnvgl.CCW                            = 0x0901;

cnvgl.LINE_WIDTH                     = 0x0B21;
cnvgl.ALIASED_POINT_SIZE_RANGE       = 0x846D;
cnvgl.ALIASED_LINE_WIDTH_RANGE       = 0x846E;
cnvgl.CULL_FACE_MODE                 = 0x0B45;
cnvgl.FRONT_FACE                     = 0x0B46;
cnvgl.DEPTH_RANGE                    = 0x0B70;
cnvgl.DEPTH_WRITEMASK                = 0x0B72;
cnvgl.DEPTH_CLEAR_VALUE              = 0x0B73;
cnvgl.DEPTH_FUNC                     = 0x0B74;
cnvgl.STENCIL_CLEAR_VALUE            = 0x0B91;
cnvgl.STENCIL_FUNC                   = 0x0B92;
cnvgl.STENCIL_FAIL                   = 0x0B94;
cnvgl.STENCIL_PASS_DEPTH_FAIL        = 0x0B95;
cnvgl.STENCIL_PASS_DEPTH_PASS        = 0x0B96;
cnvgl.STENCIL_REF                    = 0x0B97;
cnvgl.STENCIL_VALUE_MASK             = 0x0B93;
cnvgl.STENCIL_WRITEMASK              = 0x0B98;
cnvgl.STENCIL_BACK_FUNC              = 0x8800;
cnvgl.STENCIL_BACK_FAIL              = 0x8801;
cnvgl.STENCIL_BACK_PASS_DEPTH_FAIL   = 0x8802;
cnvgl.STENCIL_BACK_PASS_DEPTH_PASS   = 0x8803;
cnvgl.STENCIL_BACK_REF               = 0x8CA3;
cnvgl.STENCIL_BACK_VALUE_MASK        = 0x8CA4;
cnvgl.STENCIL_BACK_WRITEMASK         = 0x8CA5;
cnvgl.VIEWPORT                       = 0x0BA2;
cnvgl.SCISSOR_BOX                    = 0x0C10;
cnvgl.COLOR_CLEAR_VALUE              = 0x0C22;
cnvgl.COLOR_WRITEMASK                = 0x0C23;
cnvgl.UNPACK_ALIGNMENT               = 0x0CF5;
cnvgl.PACK_ALIGNMENT                 = 0x0D05;
cnvgl.MAX_TEXTURE_SIZE               = 0x0D33;
cnvgl.MAX_VIEWPORT_DIMS              = 0x0D3A;
cnvgl.SUBPIXEL_BITS                  = 0x0D50;
cnvgl.RED_BITS                       = 0x0D52;
cnvgl.GREEN_BITS                     = 0x0D53;
cnvgl.BLUE_BITS                      = 0x0D54;
cnvgl.ALPHA_BITS                     = 0x0D55;
cnvgl.DEPTH_BITS                     = 0x0D56;
cnvgl.STENCIL_BITS                   = 0x0D57;
cnvgl.POLYGON_OFFSET_UNITS           = 0x2A00;
cnvgl.POLYGON_OFFSET_FACTOR          = 0x8038;
cnvgl.TEXTURE_BINDING_2D             = 0x8069;
cnvgl.SAMPLE_BUFFERS                 = 0x80A8;
cnvgl.SAMPLES                        = 0x80A9;
cnvgl.SAMPLE_COVERAGE_VALUE          = 0x80AA;
cnvgl.SAMPLE_COVERAGE_INVERT         = 0x80AB;


cnvgl.NUM_COMPRESSED_TEXTURE_FORMATS = 0x86A2;
cnvgl.COMPRESSED_TEXTURE_FORMATS     = 0x86A3;

cnvgl.DONT_CARE                      = 0x1100;
cnvgl.FASTEST                        = 0x1101;
cnvgl.NICEST                         = 0x1102;

cnvgl.GENERATE_MIPMAP_HINT            = 0x8192;

cnvgl.BYTE                           = 0x1400;
cnvgl.UNSIGNED_BYTE                  = 0x1401;
cnvgl.SHORT                          = 0x1402;
cnvgl.UNSIGNED_SHORT                 = 0x1403;
cnvgl.INT                            = 0x1404;
cnvgl.UNSIGNED_INT                   = 0x1405;
cnvgl.FLOAT                          = 0x1406;
cnvgl.DOUBLE                         = 0x140A;
cnvgl.FIXED                          = 0x140C;

cnvgl.DEPTH_COMPONENT                = 0x1902;
cnvgl.ALPHA                          = 0x1906;
cnvgl.RGB                            = 0x1907;
cnvgl.RGBA                           = 0x1908;
cnvgl.LUMINANCE                      = 0x1909;
cnvgl.LUMINANCE_ALPHA                = 0x190A;

cnvgl.UNSIGNED_SHORT_4_4_4_4         = 0x8033;
cnvgl.UNSIGNED_SHORT_5_5_5_1         = 0x8034;
cnvgl.UNSIGNED_SHORT_5_6_5           = 0x8363;

cnvgl.FRAGMENT_SHADER                  = 0x8B30;
cnvgl.VERTEX_SHADER                    = 0x8B31;
cnvgl.MAX_VERTEX_ATTRIBS               = 0x8869;
cnvgl.MAX_VERTEX_UNIFORM_VECTORS       = 0x8DFB;
cnvgl.MAX_VARYING_VECTORS              = 0x8DFC;
cnvgl.MAX_COMBINED_TEXTURE_IMAGE_UNITS = 0x8B4D;
cnvgl.MAX_VERTEX_TEXTURE_IMAGE_UNITS   = 0x8B4C;
cnvgl.MAX_TEXTURE_IMAGE_UNITS          = 0x8872;
cnvgl.MAX_FRAGMENT_UNIFORM_VECTORS     = 0x8DFD;
cnvgl.SHADER_TYPE                      = 0x8B4F;
cnvgl.DELETE_STATUS                    = 0x8B80;
cnvgl.LINK_STATUS                      = 0x8B82;
cnvgl.VALIDATE_STATUS                  = 0x8B83;
cnvgl.ATTACHED_SHADERS                 = 0x8B85;
cnvgl.ACTIVE_UNIFORMS                  = 0x8B86;
cnvgl.ACTIVE_UNIFORM_MAX_LENGTH        = 0x8B87;
cnvgl.ACTIVE_ATTRIBUTES                = 0x8B89;
cnvgl.ACTIVE_ATTRIBUTE_MAX_LENGTH      = 0x8B8A;
cnvgl.SHADING_LANGUAGE_VERSION         = 0x8B8C;
cnvgl.CURRENT_PROGRAM                  = 0x8B8D;

cnvgl.NEVER                          = 0x0200;
cnvgl.LESS                           = 0x0201;
cnvgl.EQUAL                          = 0x0202;
cnvgl.LEQUAL                         = 0x0203;
cnvgl.GREATER                        = 0x0204;
cnvgl.NOTEQUAL                       = 0x0205;
cnvgl.GEQUAL                         = 0x0206;
cnvgl.ALWAYS                         = 0x0207;

cnvgl.KEEP                           = 0x1E00;
cnvgl.REPLACE                        = 0x1E01;
cnvgl.INCR                           = 0x1E02;
cnvgl.DECR                           = 0x1E03;
cnvgl.INVERT                         = 0x150A;
cnvgl.INCR_WRAP                      = 0x8507;
cnvgl.DECR_WRAP                      = 0x8508;

cnvgl.VENDOR                         = 0x1F00;
cnvgl.RENDERER                       = 0x1F01;
cnvgl.VERSION                        = 0x1F02;
cnvgl.EXTENSIONS                     = 0x1F03;

cnvgl.NEAREST                        = 0x2600;
cnvgl.LINEAR                         = 0x2601;

cnvgl.NEAREST_MIPMAP_NEAREST         = 0x2700;
cnvgl.LINEAR_MIPMAP_NEAREST          = 0x2701;
cnvgl.NEAREST_MIPMAP_LINEAR          = 0x2702;
cnvgl.LINEAR_MIPMAP_LINEAR           = 0x2703;

cnvgl.TEXTURE_MAG_FILTER             = 0x2800;
cnvgl.TEXTURE_MIN_FILTER             = 0x2801;
cnvgl.TEXTURE_WRAP_S                 = 0x2802;
cnvgl.TEXTURE_WRAP_T                 = 0x2803;

cnvgl.TEXTURE                        = 0x1702;

cnvgl.TEXTURE_CUBE_MAP               = 0x8513;
cnvgl.TEXTURE_BINDING_CUBE_MAP       = 0x8514;
cnvgl.TEXTURE_CUBE_MAP_POSITIVE_X    = 0x8515;
cnvgl.TEXTURE_CUBE_MAP_NEGATIVE_X    = 0x8516;
cnvgl.TEXTURE_CUBE_MAP_POSITIVE_Y    = 0x8517;
cnvgl.TEXTURE_CUBE_MAP_NEGATIVE_Y    = 0x8518;
cnvgl.TEXTURE_CUBE_MAP_POSITIVE_Z    = 0x8519;
cnvgl.TEXTURE_CUBE_MAP_NEGATIVE_Z    = 0x851A;
cnvgl.MAX_CUBE_MAP_TEXTURE_SIZE      = 0x851C;

cnvgl.TEXTURE0                       = 0x84C0;
cnvgl.TEXTURE1                       = 0x84C1;
cnvgl.TEXTURE2                       = 0x84C2;
cnvgl.TEXTURE3                       = 0x84C3;
cnvgl.TEXTURE4                       = 0x84C4;
cnvgl.TEXTURE5                       = 0x84C5;
cnvgl.TEXTURE6                       = 0x84C6;
cnvgl.TEXTURE7                       = 0x84C7;
cnvgl.TEXTURE8                       = 0x84C8;
cnvgl.TEXTURE9                       = 0x84C9;
cnvgl.TEXTURE10                      = 0x84CA;
cnvgl.TEXTURE11                      = 0x84CB;
cnvgl.TEXTURE12                      = 0x84CC;
cnvgl.TEXTURE13                      = 0x84CD;
cnvgl.TEXTURE14                      = 0x84CE;
cnvgl.TEXTURE15                      = 0x84CF;
cnvgl.TEXTURE16                      = 0x84D0;
cnvgl.TEXTURE17                      = 0x84D1;
cnvgl.TEXTURE18                      = 0x84D2;
cnvgl.TEXTURE19                      = 0x84D3;
cnvgl.TEXTURE20                      = 0x84D4;
cnvgl.TEXTURE21                      = 0x84D5;
cnvgl.TEXTURE22                      = 0x84D6;
cnvgl.TEXTURE23                      = 0x84D7;
cnvgl.TEXTURE24                      = 0x84D8;
cnvgl.TEXTURE25                      = 0x84D9;
cnvgl.TEXTURE26                      = 0x84DA;
cnvgl.TEXTURE27                      = 0x84DB;
cnvgl.TEXTURE28                      = 0x84DC;
cnvgl.TEXTURE29                      = 0x84DD;
cnvgl.TEXTURE30                      = 0x84DE;
cnvgl.TEXTURE31                      = 0x84DF;
cnvgl.ACTIVE_TEXTURE                 = 0x84E0;

cnvgl.REPEAT                         = 0x2901;
cnvgl.CLAMP_TO_EDGE                  = 0x812F;
cnvgl.MIRRORED_REPEAT                = 0x8370;

cnvgl.FLOAT_VEC2                     = 0x8B50;
cnvgl.FLOAT_VEC3                     = 0x8B51;
cnvgl.FLOAT_VEC4                     = 0x8B52;
cnvgl.INT_VEC2                       = 0x8B53;
cnvgl.INT_VEC3                       = 0x8B54;
cnvgl.INT_VEC4                       = 0x8B55;
cnvgl.BOOL                           = 0x8B56;
cnvgl.BOOL_VEC2                      = 0x8B57;
cnvgl.BOOL_VEC3                      = 0x8B58;
cnvgl.BOOL_VEC4                      = 0x8B59;
cnvgl.FLOAT_MAT2                     = 0x8B5A;
cnvgl.FLOAT_MAT3                     = 0x8B5B;
cnvgl.FLOAT_MAT4                     = 0x8B5C;
cnvgl.SAMPLER_2D                     = 0x8B5E;
cnvgl.SAMPLER_CUBE                   = 0x8B60;

cnvgl.VERTEX_ATTRIB_ARRAY_ENABLED        = 0x8622;
cnvgl.VERTEX_ATTRIB_ARRAY_SIZE           = 0x8623;
cnvgl.VERTEX_ATTRIB_ARRAY_STRIDE         = 0x8624;
cnvgl.VERTEX_ATTRIB_ARRAY_TYPE           = 0x8625;
cnvgl.VERTEX_ATTRIB_ARRAY_NORMALIZED     = 0x886A;
cnvgl.VERTEX_ATTRIB_ARRAY_POINTER        = 0x8645;
cnvgl.VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = 0x889F;

cnvgl.IMPLEMENTATION_COLOR_READ_TYPE   = 0x8B9A;
cnvgl.IMPLEMENTATION_COLOR_READ_FORMAT = 0x8B9B;

cnvgl.COMPILE_STATUS                 = 0x8B81;
cnvgl.INFO_LOG_LENGTH                = 0x8B84;
cnvgl.SHADER_SOURCE_LENGTH           = 0x8B88;
cnvgl.SHADER_COMPILER                = 0x8DFA;

cnvgl.SHADER_BINARY_FORMATS          = 0x8DF8;
cnvgl.NUM_SHADER_BINARY_FORMATS      = 0x8DF9;

cnvgl.LOW_FLOAT                      = 0x8DF0;
cnvgl.MEDIUM_FLOAT                   = 0x8DF1;
cnvgl.HIGH_FLOAT                     = 0x8DF2;
cnvgl.LOW_INT                        = 0x8DF3;
cnvgl.MEDIUM_INT                     = 0x8DF4;
cnvgl.HIGH_INT                       = 0x8DF5;

cnvgl.FRAMEBUFFER                    = 0x8D40;
cnvgl.RENDERBUFFER                   = 0x8D41;

cnvgl.RGBA4                          = 0x8056;
cnvgl.RGB5_A1                        = 0x8057;
cnvgl.RGB565                         = 0x8D62;
cnvgl.DEPTH_COMPONENT16              = 0x81A5;
cnvgl.STENCIL_INDEX                  = 0x1901;
cnvgl.STENCIL_INDEX8                 = 0x8D48;

cnvgl.RENDERBUFFER_WIDTH             = 0x8D42;
cnvgl.RENDERBUFFER_HEIGHT            = 0x8D43;
cnvgl.RENDERBUFFER_INTERNAL_FORMAT   = 0x8D44;
cnvgl.RENDERBUFFER_RED_SIZE          = 0x8D50;
cnvgl.RENDERBUFFER_GREEN_SIZE        = 0x8D51;
cnvgl.RENDERBUFFER_BLUE_SIZE         = 0x8D52;
cnvgl.RENDERBUFFER_ALPHA_SIZE        = 0x8D53;
cnvgl.RENDERBUFFER_DEPTH_SIZE        = 0x8D54;
cnvgl.RENDERBUFFER_STENCIL_SIZE      = 0x8D55;

cnvgl.FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE           = 0x8CD0;
cnvgl.FRAMEBUFFER_ATTACHMENT_OBJECT_NAME           = 0x8CD1;
cnvgl.FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL         = 0x8CD2;
cnvgl.FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 0x8CD3;

cnvgl.COLOR_ATTACHMENT0              = 0x8CE0;
cnvgl.DEPTH_ATTACHMENT               = 0x8D00;
cnvgl.STENCIL_ATTACHMENT             = 0x8D20;

cnvgl.NONE                           = 0;

cnvgl.FRAMEBUFFER_COMPLETE                      = 0x8CD5;
cnvgl.FRAMEBUFFER_INCOMPLETE_ATTACHMENT         = 0x8CD6;
cnvgl.FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 0x8CD7;
cnvgl.FRAMEBUFFER_INCOMPLETE_DIMENSIONS         = 0x8CD9;
cnvgl.FRAMEBUFFER_UNSUPPORTED                   = 0x8CDD;

cnvgl.FRAMEBUFFER_BINDING            = 0x8CA6;
cnvgl.RENDERBUFFER_BINDING           = 0x8CA7;
cnvgl.MAX_RENDERBUFFER_SIZE          = 0x84E8;

cnvgl.INVALID_FRAMEBUFFER_OPERATION  = 0x0506;




cnvgl.MAX_FRAGMENT_UNIFORM_COMPONENTS = 0x8B49;

cnvgl.TEXTURE_1D					= 0x0DE0;
cnvgl.TEXTURE_2D					= 0x0DE1;
cnvgl.TEXTURE_3D					= 0x806F;

cnvgl.READ_WRITE					= 0x88BA;

cnvgl.DEPTH_COMPONENT16				= 0x81A5;




(function(cnvgl) {


	cnvgl.attrib_array_object = function() {
		this.size			= 4;
		this.type			= 0;
		this.stride			= 0;
		this.pointer		= 0;
		this.enabled		= cnvgl.FALSE;
		this.normalized		= cnvgl.FALSE;
		this.integer		= cnvgl.FALSE;
		this.element_size	= 0;
		this.buffer_obj		= null;
	};

	}(cnvgl));




(function(cnvgl) {

	cnvgl.buffer = function() {	
		this.data = null;
		this.usage = null;
		this.target = null;
		this.access = null;
		this.size = 0;
		this.bpe = 0;
	};

}(cnvgl));



(function(cnvgl) {


	cnvgl.context = (function() {

								  		function Initializer() {

						this.driver = null;

			this.array = {};
			this.color = {};
			this.depth = {};
			this.pack = {};
			this.polygon = {};
			this.shader = {};
			this.texture = {};
			this.unpack = {};
			this.viewport = {};

			this.errorValue = 0;

			this.drawBuffer = null;

			this.winDrawBuffer = null;

			this.vertex_attrib_arrays = [];

				this.currentRenderbuffer = null;

				this.shared = null;
			this.const = {};
		}

			var cnvgl_context = jClass('cnvgl_context', Initializer);


			cnvgl_context.cnvgl_context = function(driver) {

			cnvgl.setContext(this);
			this.shared = cnvgl.context_shared.getInstance();
			this.driver = driver;

			this.array = {
				arrayBufferObj : null,
				elementArrayBufferObj : null,
				arrayObj : {
					vertexAttrib : []	
				}
			};

			this.color = {
				blendColor : [0, 0, 0, 0],
				blendDestA : cnvgl.ZERO,
				blendDestRGB : cnvgl.ZERO,
				blendEnabled : cnvgl.FALSE,
				blendEquationA : cnvgl.FUNC_ADD,
				blendEquationRGB : cnvgl.FUNC_ADD,
				blendSrcA : cnvgl.ONE,
				blendSrcRGB : cnvgl.ONE,
				clearColor : [0, 0, 0, 0],
				colorMask : [0xFF, 0xFF, 0xFF, 0xFF],
				ditherFlag : cnvgl.TRUE
			};

			this.depth = {
				clear : 1.0,
				func : cnvgl.LESS,
				mask : cnvgl.TRUE,
				test : cnvgl.FALSE
			};

			this.hint = {
				generateMipmap : cnvgl.DONT_CARE
			};

			this.line = {
				width : 1
			};

			this.multisample = {
				sampleCoverageInvert : cnvgl.FALSE,
				sampleCoverageValue : 1
			};

			this.pack = {
				alignment : 4
			};

			this.polygon = {
				cullFaceMode : cnvgl.BACK,
				cullFlag : cnvgl.FALSE,
				frontFace : cnvgl.CCW,
				offsetFactor : 0,
				offsetUnits : 0
			};

			this.scissor = {
				enabled : cnvgl.FALSE
			};

			this.shader = {
				activeProgram : null	
			};

			this.stencil = {
				clear : 0,
				failFunc : [cnvgl.KEEP, cnvgl.KEEP],
				func : [cnvgl.ALWAYS, cnvgl.ALWAYS],
				ref : [0, 0],
				valueMask : [~0, ~0],
				writeMask : [~0, ~0],
				zFailFunc : [cnvgl.KEEP, cnvgl.KEEP],
				zPassFunc : [cnvgl.KEEP, cnvgl.KEEP]
			};

			this.unpack = {
				alignment : 4
			};

			this.viewport = {
				near : 0.0,
				far : 1.0,
				x : 0,
				y : 0,
				w : 0,
				h : 0
			};

			this.errorValue = cnvgl.NO_ERROR;

						this.initConst();

			this.initFramebuffer();
			this.initTextures();
			this.initVertexAttribs();
		};

			cnvgl_context.initConst = function() {

			this.const.maxVarying = 0;

						this.const.vertexProgram = {
				maxAttribs : 0,
				maxUniformComponents : 0
			};

						this.const.fragmentProgram = {
				maxUniformComponents : 0
			};

						this.driver.initContext(this, this.const);
		};

			cnvgl_context.initFramebuffer = function(width, height) {
			var frameBuffer, colorBuffer, depthBuffer, stencilBuffer;

			frameBuffer = new cnvgl.framebuffer(0);
			frameBuffer.width = width;
			frameBuffer.height = height;

				this.winDrawBuffer = frameBuffer;
			this.drawBuffer = frameBuffer;

			colorBuffer = new cnvgl.renderbuffer(0);
			colorBuffer.internalFormat = cnvgl.RGBA;
			colorBuffer.width = width;
			colorBuffer.height = height;
			colorBuffer.data = this.driver.colorBuffer;
			frameBuffer.colorDrawBuffers[0] = colorBuffer;

			depthBuffer = new cnvgl.renderbuffer(0);
			depthBuffer.internalFormat = cnvgl.DEPTH_COMPONENT16;
			depthBuffer.width = width;
			depthBuffer.height = height;
			depthBuffer.data = this.driver.depthBuffer;
			frameBuffer.depthBuffer = depthBuffer;

			stencilBuffer = new cnvgl.renderbuffer(0);
			stencilBuffer.internalFormat = cnvgl.STENCIL_INDEX8;
			stencilBuffer.width = width;
			stencilBuffer.height = height;
			stencilBuffer.data = this.driver.stencilBuffer;
			frameBuffer.stencilBuffer = stencilBuffer;
		};

		cnvgl_context.initTextures = function() {
			var units, i, unit;
			this.texture.currentUnit = 0;
			this.texture.unit = [];
			for (i = 0; i < this.const.maxTextureUnits; i++) {
				unit = new cnvgl.texture_unit(this, i);
				unit.current_texture[cnvgl.TEXTURE_2D] = this.shared.default_texture_objects[cnvgl.TEXTURE_2D];
				this.texture.unit[i] = unit;
			}
		};

			cnvgl_context.initVertexAttribs = function() {
			var i;
			for (i = 0; i < this.const.vertexProgram.maxAttribs; i++) {
				this.array.arrayObj.vertexAttrib[i] = new cnvgl.attrib_array_object();
			}
		};


			cnvgl_context.Static.findFreeName = function(list, start) {
			start = start || 1;
			while (list[start]) {
				start++;
			}
			return start;
		};

			return cnvgl_context.Constructor;

	}());


}(cnvgl));




(function(cnvgl) {


		cnvgl.context_shared = (function() {

								  		function Initializer() {
			this.texture_objects = [0];
			this.default_texture_objects = {};

				this.bufferObjects = [0];
			this.shaderObjects = [0];
			this.frameBuffers = [0];
			this.renderBuffers = [0];
		}

			var cnvgl_context_shared = jClass('cnvgl_context_shared', Initializer);

		var instance = null;
		cnvgl_context_shared.Constructor.getInstance = function() {
			if (instance) {
				return instance;	
			}
			return new cnvgl_context_shared.Constructor();
		};


			cnvgl_context_shared.cnvgl_context_shared = function() {
			this.initTextures();
		};

				cnvgl_context_shared.initTextures = function() {
			var tex2d, tex2di;

			tex2d = new cnvgl.texture_object(0, cnvgl.TEXTURE_2D);
			this.default_texture_objects[cnvgl.TEXTURE_2D] = tex2d;
			tex2di = new cnvgl.texture_object(tex2d);
			tex2di.data = new Uint8Array([0,0,0,255]);
			tex2di.width = 1;
			tex2di.height = 1;
			tex2di.internalFormat = cnvgl.RGBA;
			tex2d.images[0] = tex2di;
		};

			return cnvgl_context_shared.Constructor;

		}());


}(cnvgl));




(function(cnvgl) {

	cnvgl.framebuffer = function(name) {

			this.name = name;
		this.refCount = 0;
		this.deletePending = false;

			this.visual = null; 
		this.initialized = null;

			this.width = 0;
		this.height = 0;

			this.xmin = 0;
		this.xmax = 0;
		this.ymin = 0;
		this.ymax = 0;

				this.depthMax = 0;
		this.depthMaxF = 0;
		this.MRD = 0;

				this.status = null;
		this.integerColor = null;

			this.attachment = [];

			this.colorDrawBuffers = [];   
		this.colorReadBuffer = null;

			this.numColorDrawBuffers = 0;
		this.colorDrawBufferIndexes = [];
		this.colorReadBufferIndex = -1;

			this.depthBuffer = null;
		this.stencilBuffer = null;
	};


}(cnvgl));




(function(cnvgl) {


		cnvgl.program = (function() {

			function var_set() {
			this.bound = {};
			this.active = [];
			this.names = {};
		}

			function Initializer() {
			this.name = 0;
			this.attached_shaders = [];

			this.delete_status = 0;
			this.link_status = 0;
			this.validate_status = 0;
			this.information_log = "";

			this.uniforms = null;
			this.attributes = null;
			this.varying = null;

			this.program = null;
		}

			var cnvgl_program = jClass('cnvgl_program', Initializer);

			cnvgl_program.cnvgl_program = function() {
			this.reset();
		};

			cnvgl_program.reset = function() {
			var bound_attr;

				this.delete_status = 0;
			this.link_status = 0;
			this.validate_status = 0;
			this.information_log = "";

				bound_attr = this.attributes ? this.attributes.bound : {};

				this.uniforms = new var_set();
			this.attributes = new var_set();
			this.varying = new var_set();

				this.attributes.bound = bound_attr;
		};

			cnvgl_program.getOpenSlot = function(set) {
			if (set.active.length == 0) {
				return 0;	
			}
			last = set.active[set.active.length - 1];
			return last.location + last.slots;
		};

		cnvgl_program.addActiveAttribute = function(attr) {
			this.attributes.active.push(attr);
			this.attributes.names[attr.name] = attr;
		};

		cnvgl_program.addActiveUniform = function(uniform) {
			this.uniforms.active.push(uniform);
			this.uniforms.names[uniform.name] = uniform;
		};

		cnvgl_program.addActiveVarying = function(varying) {
			this.varying.active.push(varying);
			this.varying.names[varying.name] = varying;
		};

		cnvgl_program.getActiveAttribute = function(name) {
			return this.attributes.names[name];
		};

			cnvgl_program.getActiveUniform = function(name) {
			return this.uniforms.names[name];
		};

			cnvgl_program.getActiveVarying = function(name) {
			return this.varying.names[name];
		};

			return cnvgl_program.Constructor;

		}());


		cnvgl.program_var = function(name, type, location, slots, components) {
		this.name = name;
		this.type = type;
		this.location = location;
		this.slots = slots;
		this.components = components;
		this.basetype = cnvgl.FLOAT;
		this.value = [0,0,0,0];
	};


	}(cnvgl));




(function(cnvgl) {


		cnvgl.shader = function(name, type) {

			this.name = name;
		this.type = type;

		this.delete_status = cnvgl.FALSE;
		this.compile_status = cnvgl.FALSE;
		this.information_log = '';
		this.shader_string = '';

		this.object_code = null;
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.renderbuffer = function(name) {
		this.name = name;

			this.width = 0;
		this.height = 0;

			this.internalFormat = null;
		this.baseFormat = null;

			this.format = 0;

			this.numSamples = null;

			this.dataType = null;
		this.data = null;

				this.wrapped = null;	
	};


	}(cnvgl));




(function(cnvgl) {


		cnvgl.texture_unit = function(ctx, unit) {
		this.unit = unit;	
		this.current_texture = {};
	};

		cnvgl.texture_object = function(name, target) {

		this.name = name;
		this.target = target;

			this.min_filter = cnvgl.NEAREST_MIPMAP_LINEAR;
		this.mag_filter = cnvgl.LINEAR;

		this.images = [];
	};


			cnvgl.texture_image = function() {	
		this.width = 0;
		this.height = 0;
		this.internalFormat = null;
	};

}(cnvgl));





(function(cnvgl) {


	cnvgl.blendColor = function(red, green, blue, alpha) {
		var ctx, c;
		ctx = cnvgl.getCurrentContext();
		c = ctx.color.blendColor;
		c[0] = Math.round(255 * Math.max(Math.min(red, 1), 0));
		c[1] = Math.round(255 * Math.max(Math.min(green, 1), 0));
		c[2] = Math.round(255 * Math.max(Math.min(blue, 1), 0));
		c[3] = Math.round(255 * Math.max(Math.min(alpha, 1), 0));		

		ctx.driver.blendColor(ctx, red, green, blue, alpha);
	};

	cnvgl.blendEquation = function(mode) {
		cnvgl.blendEquationSeparate(mode, mode);
	};

	cnvgl.blendEquationSeparate = function(modeRGB, modeAlpha) {
		var ctx;

		ctx = cnvgl.getCurrentContext();


		if (ctx.color.blendEquationRGB == modeRGB && ctx.color.blendEquationA == modeAlpha) {
			return;	
		}

		ctx.color.blendEquationRGB = modeRGB;
		ctx.color.blendEquationA = modeAlpha;

		ctx.driver.blendEquationSeparate(ctx, modeRGB, modeAlpha);
	};

	cnvgl.blendFunc = function(sfactor, dfactor) {
		var ctx, i;

		ctx = cnvgl.getCurrentContext();


				ctx.color.blendSrcRGB = sfactor;
		ctx.color.blendSrcA = sfactor;
		ctx.color.blendDestRGB = dfactor;
		ctx.color.blendDestA = dfactor;

				ctx.driver.blendFunc(ctx, sfactor, dfactor);
	};

	cnvgl.colorMask = function(r, g, b, a) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

			ctx.color.colorMask = [
			r ? 0xFF : 0,
			g ? 0xFF : 0,
			b ? 0xFF : 0,
			a ? 0xFF : 0
		];

		ctx.driver.colorMask(ctx, r, g, b, a);
	};


		}(cnvgl));




(function(cnvgl) {


	cnvgl.bindBuffer = function(target, buffer) {
		var ctx, buffer_obj;

		ctx = cnvgl.getCurrentContext();

		if (buffer != 0) {

			buffer_obj = ctx.shared.bufferObjects[buffer];

			if (!buffer_obj) {
				cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
				return;
			}

			if (!buffer_obj instanceof cnvgl.buffer) {
				cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
				return;
			}

			buffer_obj.access = cnvgl.READ_WRITE;
			buffer_obj.usage = cnvgl.STATIC_DRAW;

			} else {
			buffer_obj = null;	
		}

			switch (target) {
			case cnvgl.ARRAY_BUFFER:
				ctx.array.arrayBufferObj = buffer_obj;
				break;
			case cnvgl.ELEMENT_ARRAY_BUFFER:
				ctx.array.elementArrayBufferObj = buffer_obj;
				break;
			default:
				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
		}
	};


	cnvgl.bufferData = function(target, size, data, usage) {
		var ctx, buffer_obj, data_type, view, temp, i;

		ctx = cnvgl.getCurrentContext();

			if (usage != cnvgl.STREAM_DRAW &&
				usage != cnvgl.STREAM_READ && 
				usage != cnvgl.STREAM_COPY &&
				usage != cnvgl.STATIC_DRAW &&
				usage != cnvgl.STATIC_READ &&
				usage != cnvgl.STATIC_COPY &&
				usage != cnvgl.DYNAMIC_DRAW &&
				usage != cnvgl.DYNAMIC_READ && 
				usage != cnvgl.DYNAMIC_COPY) {

				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

			if (size < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			switch (target) {
			case cnvgl.ARRAY_BUFFER:
				buffer_obj = ctx.array.arrayBufferObj;
				break;
			case cnvgl.ELEMENT_ARRAY_BUFFER:
				buffer_obj = ctx.array.elementArrayBufferObj;
				break;
			default:
				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
				return;
		}

				if (!buffer_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		buffer_obj.target = target;
		buffer_obj.usage = usage;
		buffer_obj.size = size;

		if (data) {
			buffer_obj.bpe = data.constructor.BYTES_PER_ELEMENT;
			size /= buffer_obj.bpe;
			buffer_obj.data = cnvgl.malloc(size);
			cnvgl.memcpy(buffer_obj.data, 0, data, size, 0);
		}
	};


	cnvgl.bufferSubData = function(target, offset, size, data) {
		var ctx, buffer_obj, i;

		ctx = cnvgl.getCurrentContext();

		switch (target) {
			case cnvgl.ARRAY_BUFFER:
				buffer_obj = ctx.array.arrayBufferObj;
				break;
			case cnvgl.ELEMENT_ARRAY_BUFFER:
				bufer_obj = ctx.array.elementArrayBufferObj;
				break;
			default:
				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
				return;
		}

		if (!buffer_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		if (offset < 0 || size < 0 || offset + size > buffer_obj.size) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!buffer_obj.data) {
			buffer_obj.bpe = data.constructor.BYTES_PER_ELEMENT;
			buffer_obj.data = cnvgl.malloc(buffer_obj.size / buffer_obj.bpe);
		}

		size /= buffer_obj.bpe;
		offset /= buffer_obj.bpe;

		for (i = 0; i < size; i++) {
			buffer_obj.data[offset + i] = data[i];
		}
	};


	cnvgl.genBuffers = function(n, buffers) {
		var ctx, list, buffer_obj, name, i;

		ctx = cnvgl.getCurrentContext();
		list = [];

		for (i = 0; i < n; i++) {
			buffer_obj = new cnvgl.buffer();
			name = cnvgl.context.findFreeName(ctx.shared.bufferObjects);
			ctx.shared.bufferObjects[name] = buffer_obj;			
			list.push(name);
		}

		buffers[0] = list;
	}


}(cnvgl));




(function(cnvgl) {


	cnvgl.clearColor = function(red, green, blue, alpha) {
		var ctx, c;
		ctx = cnvgl.getCurrentContext();
		c = ctx.color.clearColor;
		c[0] = Math.round(255 * Math.max(Math.min(red, 1), 0));
		c[1] = Math.round(255 * Math.max(Math.min(green, 1), 0));
		c[2] = Math.round(255 * Math.max(Math.min(blue, 1), 0));
		c[3] = Math.round(255 * Math.max(Math.min(alpha, 1), 0));
	};


	cnvgl.clear = function(mask) {
		var ctx;

		ctx = cnvgl.getCurrentContext();

		if (mask & ~(cnvgl.COLOR_BUFFER_BIT | cnvgl.DEPTH_BUFFER_BIT | cnvgl.STENCIL_BUFFER_BIT)) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		ctx.driver.clear(ctx, ctx.color.clearColor, ctx.depth.clear, ctx.stencil.clear, mask);
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.flush = function() {
		var ctx;
		ctx = cnvgl.getCurrentContext();
		ctx.driver.flush(ctx);
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.clearDepth = function(depth) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

			depth = Math.max(Math.min(depth, 1), 0);
		ctx.depth.clear = depth;
	};


	cnvgl.depthFunc = function(func) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		if (func != cnvgl.NEVER
			&& func != cnvgl.LESS
			&& func != cnvgl.EQUAL
			&& func != cnvgl.LEQUAL
			&& func != cnvgl.GREATER
			&& func != cnvgl.NOTEQUAL
			&& func != cnvgl.GEQUAL
			&& func != cnvgl.ALWAYS) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;		
		}

		ctx.depth.func = func;

		ctx.driver.depthFunc(ctx, func);
	};


	cnvgl.depthMask = function(mask) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

			ctx.depth.mask = mask ? cnvgl.TRUE : cnvgl.FALSE;

				ctx.driver.depthMask(ctx, mask);
	};


}(cnvgl));




(function(cnvgl) {

	function uploadAttributes(ctx) {
		var program_obj, array_objs, array_obj, a, attrib_objs, attrib_obj, data, buffer_obj, data;

		program_obj = ctx.shader.activeProgram;
		array_objs = ctx.array.arrayObj.vertexAttrib;
		attrib_objs = program_obj.attributes.active;

		for (a = 0; a < program_obj.attributes.active.length; a++) {

			attrib_obj = attrib_objs[a];
			array_obj = array_objs[attrib_obj.location];

			ctx.driver.uploadAttributes(ctx,
										attrib_obj.location,
										array_obj.size,
										array_obj.stride,
										array_obj.pointer,
										array_obj.buffer_obj.data);
		}		
	}

	cnvgl.drawArrays = function(mode, first, count) {
		var ctx;
		ctx = cnvgl.getCurrentContext();
		uploadAttributes(ctx);
		ctx.driver.drawArrays(ctx, mode, first, count);
	};

	cnvgl.drawElements = function(mode, count, type, indices) {
		var ctx;
		ctx = cnvgl.getCurrentContext();
		uploadAttributes(ctx);
		ctx.driver.drawElements(ctx, mode, indices, count, type);
	};

		}(cnvgl));




(function(cnvgl) {


	function cnvgl_enable_disable(cap, s) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		switch (cap) {

			case cnvgl.CULL_FACE:
				ctx.polygon.cullFlag = s;
				break;

			case cnvgl.DEPTH_TEST:
				ctx.depth.test = s;
				break;

			case cnvgl.BLEND:
				ctx.color.blendEnabled = s;
				break;

							case cnvgl.DITHER:
				ctx.color.ditherFlag = s;
				break;

						case cnvgl.SCISSOR_TEST:
				ctx.scissor.enabled = s;
				break;

			default:
				throw new Error('Enable/Disable for ' + cap + ' not implemented yet');
		}

		ctx.driver.enable(ctx, cap, (s == cnvgl.TRUE));
	}


	cnvgl.enable = function(cap) {
		cnvgl_enable_disable(cap, cnvgl.TRUE);
	};


	cnvgl.disable = function(cap) {
		cnvgl_enable_disable(cap, cnvgl.FALSE);
	};


}(cnvgl));




(function(cnvgl) {

	function framebuffer_texture(ctx, mode, target, attachment, textarget, texture, level, offset) {
		var fb_obj, tex_obj;

				fb_obj = ctx.drawBuffer;

				if (fb_obj.name == 0) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		tex_obj = ctx.shared.texture_objects[texture];

		ctx.driver.renderTexture(ctx, fb_obj, tex_obj, textarget, level, offset);
	}

	function cnvgl_get_attachment(ctx, fb_obj, attachment) {
		var i;
		switch (attachment) {
			case cnvgl.COLOR_ATTACHMENT0:
				i = attachment - cnvgl.COLOR_ATTACHMENT0;
				return fb_obj.attachment[BUFFER_COLOR0 + i];
			case cnvgl.DEPTH_STENCIL_ATTACHMENT:
			case cnvgl.DEPTH_BUFFER:
				return fb.Attachment[BUFFER_DEPTH];
			case cnvgl.STENCIL_BUFFER:
				return fb.attachment[BUFFER_STENCIL];
			default:
				return null;
		}
	}


	cnvgl.bindFramebuffer = function(target, framebuffer) {
		var ctx, framebuffer_obj;

			ctx = cnvgl.getCurrentContext();

			if (target != cnvgl.FRAMEBUFFER) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

			if (framebuffer) {
			framebuffer_obj = ctx.shared.frameBuffers[framebuffer];
		} else {
			framebuffer_obj = ctx.winDrawBuffer;
		}

			if (!framebuffer_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;		
		}

		if (framebuffer_obj == 1) {
			framebuffer_obj = new cnvgl.framebuffer(framebuffer);
			ctx.shared.frameBuffers[framebuffer] = framebuffer_obj;
		}

			ctx.drawBuffer = framebuffer_obj;
	};


	cnvgl.bindRenderbuffer = function(target, renderbuffer) {
		var ctx, renderbuffer_obj, name;

			ctx = cnvgl.getCurrentContext();

			if (target != cnvgl.RENDERBUFFER) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

			if (renderbuffer) {
			renderbuffer_obj = ctx.shared.renderBuffers[renderbuffer];
		}

			if (!renderbuffer_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;		
		}

			if (renderbuffer_obj == 1) {
			renderbuffer_obj = new cnvgl.renderbuffer(renderbuffer);
			ctx.shared.renderBuffers[renderbuffer] = renderbuffer_obj;
		}

			ctx.currentRenderbuffer = renderbuffer_obj;
	};


	cnvgl.framebufferRenderbuffer = function(target, attachment, renderbuffertarget, renderbuffer) {
		var ctx, att, fb_obj, rb_obj;
		ctx = cnvgl.getCurrentContext();

		if (target != cnvgl.FRAMEBUFFER) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

		if (renderbuffertarget != cnvgl.RENDERBUFFER) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

				fb_obj = ctx.drawBuffer;

		if (fb_obj.name == 0) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		att = cnvgl_get_attachment(ctx, fb_obj, attachment);

				if (!att) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

		if (renderbuffer) {
			rb_obj = _mesa_lookup_renderbuffer(ctx, renderbuffer);

						if (rb_obj == null) {
				cnvgl.throw_error(cnvgl.INVALID_OPERATION);
				return;
			}

						if (rb_obj == 0) {
				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
				return;
			}
		} else {
			rb_obj = null;
		}

		ctx.driver.framebufferRenderbuffer(ctx, fb_obj, attachment, rb_obj);
	};

	cnvgl.framebufferTexture2D = function(target, attachment, textarget, texture, level) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		framebuffer_texture(ctx, "2D", target, attachment, textarget, texture, level, 0);
	};

	cnvgl.genFramebuffers = function(n, framebuffers) {
		var ctx, list, name;

		ctx = cnvgl.getCurrentContext();
		list = [];

			if (n < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			for (i = 0; i < n; i++) {
			name = cnvgl.context.findFreeName(ctx.shared.frameBuffers, name);
			ctx.shared.frameBuffers[name] = 1;
			list.push(name);
		}

			framebuffers[0] = list;
	};


	cnvgl.genRenderbuffers = function(n, renderbuffers) {
		var ctx, list, name;

			ctx = cnvgl.getCurrentContext();
		list = [];

			if (n < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			for (i = 0; i < n; i++) {
			name = cnvgl.context.findFreeName(ctx.shared.renderBuffers, name);
			ctx.shared.renderBuffers[name] = 1;
			list.push(name);
		}

			renderbuffers[0] = list;
	};


	cnvgl.renderbufferStorage = function(target, internalFormat, width, height) {
		var ctx, renderbuffer_obj, baseFormat, temp;	

			ctx = cnvgl.getCurrentContext();

			if (target != cnvgl.RENDERBUFFER) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}


				if (width < 1 || width > ctx.const.maxRenderbufferSize) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			if (height < 1 || height > ctx.const.maxRenderbufferSize) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			renderbuffer_obj = ctx.currentRenderbuffer;

			if (!renderbuffer_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;		
		}

			if (renderbuffer_obj.internalFormat == internalFormat &&
			renderbuffer_obj.width == width &&
			renderbuffer_obj.height == height) {
			return;
		}

			renderbuffer_obj.format = 0;
		renderbuffer_obj.numSamples = 0;

			renderbuffer_obj.data = cnvgl.malloc(internalFormat, width * height);

			renderbuffer_obj.internalFormat = internalFormat;
		renderbuffer_obj.width = width;
		renderbuffer_obj.height = height;
	};

	}(cnvgl));



(function(cnvgl) {


	function cnvgl_get(pname, params) {
		var ctx;

		ctx = cnvgl.getCurrentContext();

		switch (pname) {

			case cnvgl.MAX_FRAGMENT_UNIFORM_VECTORS:
				params[0] = ctx.const.fragmentProgram.maxUniformComponents;
				break;

			case cnvgl.MAX_VARYING_VECTORS:
				params[0] = ctx.const.maxVarying;
				return;

			case cnvgl.MAX_VERTEX_ATTRIBS:
				params[0] = ctx.const.vertexProgram.maxAttribs;
				return;

			case cnvgl.MAX_VERTEX_UNIFORM_VECTORS:
				params[0] = ctx.const.vertexProgram.maxUniformComponents;
				break;

			default:
				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
				return;
		}
	}


	cnvgl.getError = function() {
		var ctx, error;
		ctx = cnvgl.getCurrentContext();
		error = ctx.errorValue;
		ctx.errorValue = cnvgl.NO_ERROR;
		return error;
	};


	cnvgl.getBooleanv = function(pname, params) {
		cnvgl_get(pname, params);
		params[0] = (params[0] == 0.0 ? cnvgl.FALSE : cnvgl.TRUE);
	};


	cnvgl.getDoublev = function(pname, params) {
		cnvgl_get(pname, params);
		if (typeof params[0] == 'boolean') {
			params[0] = params[0] ? cnvgl.TRUE : cnvgl.FALSE;	
		}
	};


	cnvgl.getFloatv = function(pname, params) {
		cnvgl_get(pname, params);
		if (typeof params[0] == 'boolean') {
			params[0] = params[0] ? cnvgl.TRUE : cnvgl.FALSE;	
		}
	};


	cnvgl.getIntegerv = function(pname, params) {
		cnvgl_get(pname, params);
		if (typeof params[0] == 'boolean') {
			params[0] = params[0] ? cnvgl.TRUE : cnvgl.FALSE;	
		} else {
			params[0] = Math.round(params[0]);
		}
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.hint = function(target, mode) {
		var ctx, name;
		ctx = cnvgl.getCurrentContext();

			if (mode != cnvgl.NICEST && mode != cnvgl.FASTEST && mode != cnvgl.DONT_CARE) {
			cnvgl.throw_error(ctx, cnvgl.INVALID_ENUM);
			return;
		}

		switch (target) {
			case cnvgl.GENERATE_MIPMAP_HINT:
				name = 'generateMipmap';
				break;

			default:
				throw new Error('hint not implemented yet ' + target);
				return;
		}

				if (ctx.hint[name] == mode) {
			return;	
		}

		ctx.hint[name] = mode;

		ctx.driver.hint(ctx, target, mode);
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.lineWidth = function(width) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

				if (width < 0) {
			cnvgl.throw_error(ctx, cnvgl.INVALID_VALUE);	
			return;
		}

				if (ctx.line.width == width) {
			return;	
		}

		ctx.line.width = width;

		ctx.driver.lineWidth(ctx, width);
	};


	}(cnvgl));




(function(cnvgl) {


	cnvgl.sampleCoverage = function(value, invert) {
		var ctx, name;
		ctx = cnvgl.getCurrentContext();

		ctx.multisample.sampleCoverageValue = Math.round(Math.max(Math.min(value, 1), 0));
		ctx.multisample.sampleCoverageInvert = invert;

		ctx.driver.sampleCoverage(ctx, value, invert);
	};


}(cnvgl));




(function(cnvgl) {


		  	function cnvgl_pixelStore(pname, param) {
		var ctx;

		ctx = cnvgl.getCurrentContext();

		switch (pname) {

			case cnvgl.PACK_ALIGNMENT:
				param = Math.round(param);
				if ([1, 2, 4, 8].indexOf(param) == -1) {
					cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
					return;
				}
				ctx.pack.alignment = param;
				break;

			case cnvgl.UNPACK_ALIGNMENT:
				param = Math.round(param);
				if ([1, 2, 4, 8].indexOf(param) == -1) {
					cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
					return;
				}
				ctx.unpack.alignment = param;
				break;

			default:
				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
				return;
		}
	}


	cnvgl.pixelStoref = function(pname, param) {
		cnvgl_pixelStore(pname, param);
	};  


	cnvgl.pixelStorei = function(pname, param) {
		cnvgl_pixelStore(pname, param);
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.cullFace = function(mode) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

				if (mode != cnvgl.FRONT_AND_BACK
			&& mode != cnvgl.FRONT
			&& mode != cnvgl.BACK) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

			ctx.polygon.cullFaceMode = mode;

				ctx.driver.cullFace(ctx, mode);
	};


	cnvgl.frontFace = function(mode) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

			if (mode != cnvgl.CW
			&& mode != cnvgl.CCW) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

			ctx.polygon.frontFace = mode;

				ctx.driver.frontFace(ctx, mode);
	};


	cnvgl.polygonOffset = function(factor, units) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

			if (ctx.polygon.offsetFactor == factor &&
			ctx.polygon.offsetUnits == units) {
			return;	
		}

		ctx.polygon.offsetFactor = factor;
		ctx.polygon.offsetUnits = units;

				ctx.driver.polygonOffset(ctx, factor, units);
	};


	}(cnvgl));




(function(cnvgl) {


	cnvgl.scissor = function(x, y, width, height) {
		var ctx, name;
		ctx = cnvgl.getCurrentContext();

		if (width < 0 || height < 0) {
			cnvgl.throw_error(ctx, cnvgl.INVALID_VALUE);
			return;
		}

		ctx.scissor.x = x;
		ctx.scissor.y = y;
		ctx.scissor.width = width;
		ctx.scissor.height = height;

				ctx.driver.scissor(ctx, x, y, width, height);
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.attachShader = function(program, shader) {
		var ctx, program_obj, shader_obj;

			ctx = cnvgl.getCurrentContext();

			program_obj = ctx.shared.shaderObjects[program];
		shader_obj = ctx.shared.shaderObjects[shader];

		if (!program_obj || !shader_obj) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!program_obj instanceof cnvgl.program || !shader_obj instanceof cnvgl.shader) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

			program_obj.attached_shaders.push(shader_obj);	
	};


	cnvgl.bindAttribLocation = function(program, index, name) {
		var ctx, program_obj, attr_obj;

			ctx = cnvgl.getCurrentContext();
		program_obj = ctx.shared.shaderObjects[program];

		if (!program_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

			if (!program_obj.attributes.bound[name]) {
			program_obj.attributes.bound[name] = index;
		}
	};


	cnvgl.getAttribLocation = function(program, name) {
		var ctx, program_obj, attr_obj;

		ctx = cnvgl.getCurrentContext();
		program_obj = ctx.shared.shaderObjects[program];

		if (!program_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

			if (!program_obj.link_status) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

			if (attrib_obj = program_obj.getActiveAttribute(name)) {
			return attrib_obj.location;	
		}

			return -1;
	};


	cnvgl.compileShader = function(shader) {
		var ctx, shader_obj, target, status;

			ctx = cnvgl.getCurrentContext();
		shader_obj = ctx.shared.shaderObjects[shader];

		if (!shader_obj) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!shader_obj instanceof cnvgl.shader) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		ctx.driver.compileShader(ctx, shader_obj.driverObj, shader_obj.shader_string, shader_obj.type);
		shader_obj.compile_status = ctx.driver.compileStatus;
		shader_obj.information_log = ctx.driver.compileLog;
	};


	cnvgl.createProgram = function() {
		var ctx, program_obj, name;

		ctx = cnvgl.getCurrentContext();
		name = ctx.shared.shaderObjects.length;

			program_obj = new cnvgl.program();
		program_obj.name = name;

			ctx.shared.shaderObjects.push(program_obj);

		program_obj.driverObj = ctx.driver.createProgram(ctx);

		return name;
	};


	cnvgl.createShader = function(shaderType) {
		var ctx, shader_obj, name;

		ctx = cnvgl.getCurrentContext();

		if (shaderType != cnvgl.FRAGMENT_SHADER && shaderType != cnvgl.VERTEX_SHADER) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return 0;
		}

		name = ctx.shared.shaderObjects.length;

			shader_obj = new cnvgl.shader(name, shaderType);	
		ctx.shared.shaderObjects.push(shader_obj);

		shader_obj.driverObj = ctx.driver.createShader(ctx, shaderType);

		return shader_obj.name;
	};


	cnvgl.getProgramiv = function(program, pname, params) {
		var ctx, program_obj, t, i;

		ctx = cnvgl.getCurrentContext();
		program_obj = ctx.shared.shaderObjects[program];

		if (!program_obj) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!program_obj instanceof cnvgl.program) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}	

			switch (pname) {

				case cnvgl.DELETE_STATUS:
				params[0] = program_obj.delete_status ? cnvgl.TRUE : cnvgl.FALSE;
				break;

						case cnvgl.LINK_STATUS:
				params[0] = program_obj.link_status;
				break;

						case cnvgl.VALIDATE_STATUS:
				params[0] = program_obj.validate_status ? cnvgl.TRUE : cnvgl.FALSE;
				break;

						case cnvgl.INFO_LOG_LENGTH:
				params[0] = program_obj.information_log.length;
				break;

				case cnvgl.ATTACHED_SHADERS:
				params[0] = program_obj.attached_shaders.length;
				break;

						case cnvgl.ACTIVE_ATTRIBUTES:
				params[0] = program_obj.attributes.active.length;
				break;

				case cnvgl.ACTIVE_ATTRIBUTE_MAX_LENGTH:
				params[0] = 0;
				for (i in program_obj.attributes.names) {
					params[0] = Math.max(params[0], i.length);
				}
				break;

				case cnvgl.ACTIVE_UNIFORMS:
				params[0] = program_obj.uniforms.active.length;
				break;

				case cnvgl.ACTIVE_UNIFORM_MAX_LENGTH:
				params[0] = 0;
				for (i in program_obj.uniforms.names) {
					params[0] = Math.max(params[0], i.length);					
				}
				break;

				default:
				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
				return;
		}
	};


	cnvgl.getShaderInfoLog = function(shader, maxLength, length, infoLog) {
		var ctx, shader_obj;

		ctx = cnvgl.getCurrentContext();
		shader_obj = ctx.shared.shaderObjects[shader];

		if (!shader_obj) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!shader_obj instanceof cnvgl.shader) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

			if (maxLength < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			var log = shader_obj.information_log;

				if (maxLength && maxLength < log.length) {
			log = log.substring(0, maxLength);
		}

			length[0] = log.length;
		infoLog[0] = log;
	};


	cnvgl.getShaderiv = function(shader, pname, params) {
		var ctx, shader_obj;

		ctx = cnvgl.getCurrentContext();
		shader_obj = ctx.shared.shaderObjects[shader];

		if (!shader_obj) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!shader_obj instanceof cnvgl.shader) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}	

			switch (pname) {
			case cnvgl.SHADER_TYPE:
				params[0] = shader_obj.type;
				break;
			case cnvgl.DELETE_STATUS:
				params[0] = shader_obj.delete_status ? cnvgl.TRUE : cnvgl.FALSE;
				break;
			case cnvgl.COMPILE_STATUS:
				params[0] = shader_obj.compile_status ? cnvgl.TRUE : cnvgl.FALSE;
				break;
			case cnvgl.INFO_LOG_LENGTH:
				params[0] = shader_obj.information_log.length;
				break;
			case cnvgl.SHADER_SOURCE_LENGTH:
				params[0] = shader_obj.shader_string.length;
				break;
			default:
				cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
				return;
		}
	};


	cnvgl.linkProgram = function(program) {
		var ctx, program_obj, i, shaders, attrib, unif, attrib_obj, uniform_obj, driver_obj;

		ctx = cnvgl.getCurrentContext();
		program_obj = ctx.shared.shaderObjects[program];

		if (!program_obj) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!program_obj instanceof cnvgl.program) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		shaders = [];
		for (i = 0; i < program_obj.attached_shaders.length; i++) {
			shaders.push(program_obj.attached_shaders[i].driverObj);
		}

		ctx.driver.link(ctx, program_obj.driverObj, shaders);
		program_obj.link_status = ctx.driver.linkStatus;
		program_obj.information_log = ctx.driver.linkErrors;

		if (program_obj.link_status) {

			driver_obj = program_obj.driverObj;

			for (i in driver_obj.attributes) {
				attrib = driver_obj.attributes[i];
				attrib_obj = new cnvgl.program_var(attrib.name, attrib.type, attrib.pos, attrib.slots, attrib.components);
				program_obj.addActiveAttribute(attrib_obj);
			}

			for (i in driver_obj.uniforms) {
				unif = driver_obj.uniforms[i];
				uniform_obj = new cnvgl.program_var(unif.name, unif.type, unif.pos, unif.slots, unif.components);
				program_obj.addActiveUniform(uniform_obj);
			}	
		}

	};


	cnvgl.shaderSource = function(shader, count, string, length) {
		var ctx, shader_obj;

			ctx = cnvgl.getCurrentContext();
		shader_obj = ctx.shared.shaderObjects[shader];

		if (!shader_obj) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!shader_obj instanceof cnvgl.shader) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		shader_obj.shader_string = string.join();
	};


	cnvgl.useProgram = function(program) {
		var ctx, program_obj, i, shader_obj;

			ctx = cnvgl.getCurrentContext();

			if (program == 0) {
			ctx.shader.activeProgram = null;
			ctx.driver.useProgram(ctx, 0);
			return;
		}

		program_obj = ctx.shared.shaderObjects[program];

		if (!program_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		if (!program_obj instanceof cnvgl.program) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

			if (!program_obj.link_status) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		ctx.shader.activeProgram = program_obj;	

				ctx.driver.useProgram(ctx, program_obj.driverObj);
	};


}(cnvgl));




(function(cnvgl) {


	function validate_stencil_func(ctx, func) {
		switch (func) {
			case cnvgl.NEVER:
			case cnvgl.LESS:
			case cnvgl.LEQUAL:
			case cnvgl.GREATER:
			case cnvgl.GEQUAL:
			case cnvgl.EQUAL:
			case cnvgl.NOTEQUAL:
			case cnvgl.ALWAYS:
				return true;
			default:
				return false;
		}
	}


		function validate_stencil_op(ctx, op) {
		switch (op) {
			case cnvgl.KEEP:
			case cnvgl.ZERO:
			case cnvgl.REPLACE:
			case cnvgl.INCR:
			case cnvgl.DECR:
			case cnvgl.INVERT:
			case cnvgl.INCR_WRAP:
			case cnvgl.DECR_WRAP:
				return true;
			default:
				return false;
		}
	}


	cnvgl.clearStencil = function(s) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		ctx.stencil.clear = s;
	};


	cnvgl.stencilFunc = function(func, ref, mask) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		if (!validate_stencil_func(ctx, func)) {
			cnvgl.throw_error(ctx, cnvgl.INVALID_ENUM);
			return;
		}

		ref = Math.max(Math.min(ref, 0xFF), 0);

		ctx.stencil.func[0] = ctx.stencil.func[1] = func;
		ctx.stencil.ref[0] = ctx.stencil.ref[1] = ref;
		ctx.stencil.valueMask[0] = ctx.stencil.valueMask = mask;

		ctx.driver.stencilFunc(ctx, func, ref, mask);
	};


	cnvgl.stencilMask = function(mask) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		ctx.stencil.writeMask[0] = ctx.stencil.writeMask[1] = mask;

		ctx.driver.stencilMask(ctx, mask);
	};


	cnvgl.stencilOp = function(sfail, dpfail, dppass) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		if (!validate_stencil_op(ctx, sfail)) {
			cnvgl.throw_error(ctx, cnvgl.INVALID_ENUM);
			return;
		}
		if (!validate_stencil_op(ctx, dpfail)) {
			cnvgl.throw_error(ctx, cnvgl.INVALID_ENUM);
			return;
		}
		if (!validate_stencil_op(ctx, dppass)) {
			cnvgl.throw_error(ctx, cnvgl.INVALID_ENUM);
			return;
		}

		ctx.stencil.failFunc[0]  = ctx.stencil.failFunc[1]  = sfail;
		ctx.stencil.zFailFunc[0] = ctx.stencil.zFailFunc[1] = dpfail;
		ctx.stencil.zPassFunc[0] = ctx.stencil.zPassFunc[1] = dppass;

		ctx.driver.stencilOp(ctx, sfail, dpfail, dppass);
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.texImage2D = function(target, level, internalFormat, width, height, border, format, type, data) {
		var ctx;

		ctx = cnvgl.getCurrentContext();

		teximage(ctx, 2, target, level, internalFormat, width, height, 1, border, format, type, data);
	};




	function teximage(ctx, dims, target, level, internalFormat, width, height, depth, border, format, type, data) {

		var unit, texture_unit, texture_obj, texture_img, size, a, n, s, k, size, group, j, src, dest;

			if (target != cnvgl.TEXTURE_1D
			&& target != cnvgl.TEXTURE_2D
			&& target != cnvgl.TEXTURE_3D
			&& target != cnvgl.TEXTURE_CUBE_MAP) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

		unit = ctx.texture.currentUnit;
		texture_unit = ctx.texture.unit[unit];
		texture_obj = texture_unit.current_texture[target];

		texture_img = new cnvgl.texture_image();

		texture_img.width = width;
		texture_img.height = height;
		texture_img.internalFormat = internalFormat;
		texture_obj.images[level] = texture_img;

		ctx.driver.texImage2D(ctx, target, level, internalFormat, width, height, depth, border, format, type, data, ctx.Unpack, texture_obj, texture_img);	
	}

}(cnvgl));




(function(cnvgl) {


	cnvgl.bindTexture = function(target, texture) {
		var ctx, unit, texture_unit, texture_obj;

			if (target != cnvgl.TEXTURE_1D &&
			target != cnvgl.TEXTURE_2D &&
			target != cnvgl.TEXTURE_3D &&
			target != cnvgl.TEXTURE_CUBE_MAP
			) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

			ctx = cnvgl.getCurrentContext();
		unit = ctx.texture.currentUnit;
		texture_unit = ctx.texture.unit[unit];

			if (texture == 0) {
			texture_obj = ctx.shared.default_texture_objects[cnvgl.TEXTURE_2D];
		} else {
			texture_obj = ctx.shared.texture_objects[texture];
			if (texture_obj) {
				if (texture_obj.target != 0 && texture_obj.target != target) {
					cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);	
					return;
				}
				texture_obj.target = target;
			} else {
				texture_obj = new cnvgl.texture_object(texture, target);
				ctx.shared.texture_objects[texture] = texture_obj;
			}
		}

			texture_unit.current_texture[target] = texture_obj;

		ctx.driver.bindTexture(ctx, target, texture_obj);
	};


	cnvgl.genTextures = function(n, textures) {
		var ctx, current, list, i, t, texture_obj;

			if (n < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			ctx = cnvgl.getCurrentContext();
		current = ctx.shared.texture_objects;

			list = [];
		for (i = 0; i < n; i++) {
			t = current.indexOf(null);
			if (t == -1) {
				t = current.length;
			}

			texture_obj = new cnvgl.texture_object(t, 0);
			texture_obj.driverObj = ctx.driver.newTextureObject();

			current[t] = texture_obj;
			list[i] = t;
		}

			textures[0] = list;
	};


		}(cnvgl));




(function(cnvgl) {


		function cnvgl_texParameter(target, pname, param) {
		var ctx, unit, texture_unit, texture_obj;

				if (target != cnvgl.TEXTURE_1D
			&& target != cnvgl.TEXTURE_2D
			&& target != cnvgl.TEXTURE_3D
			&& target != cnvgl.TEXTURE_CUBE_MAP) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

			ctx = cnvgl.getCurrentContext();
		unit = ctx.texture.currentUnit;
		texture_unit = ctx.texture.unit[unit];
		texture_obj = texture_unit.current_texture[target];

			switch (pname) {

			case cnvgl.TEXTURE_MIN_FILTER:
				texture_obj.min_filter = param;
				break;

			case cnvgl.TEXTURE_MAG_FILTER:
				texture_obj.mag_filter = param;
				break;
		}

		ctx.driver.texParameter(ctx, target, texture_obj, pname, param);
	}


	cnvgl.texParameterf = function(target, pname, param) {
		cnvgl_texParameter(target, pname, param);
	};


	cnvgl.texParameteri = function(target, pname, param) {
		cnvgl_texParameter(target, pname, param);
	};


	}(cnvgl));




(function(cnvgl) {


	cnvgl.activeTexture = function(texture) {
		var ctx, i;

		ctx = cnvgl.getCurrentContext();
		i = texture - cnvgl.TEXTURE0;

		if (i < 0 || i > (ctx.const.maxTextureUnits - 1)) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;
		}

		ctx.texture.currentUnit = i;
		if (!ctx.texture.unit[i]) {
			ctx.texture.unit[i] = new cnvgl.texture_unit(texture);	
		}
	}


}(cnvgl));




(function(cnvgl) {


	function cnvgl_uniform(location, value, slots, components) {
		var ctx, program_obj, uniform_obj, i;

			ctx = cnvgl.getCurrentContext();
		program_obj = ctx.shader.activeProgram;

			if (!program_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

		if (location == -1) {
			return;
		}

		for (i = 0; i < program_obj.uniforms.active.length; i++) {
			if (program_obj.uniforms.active[i].location == location) {
				uniform_obj = program_obj.uniforms.active[i];
				break;
			}
		}

		if (!uniform_obj) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;		
		}

		if (slots != uniform_obj.slots || components != uniform_obj.components) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;		
		}

		ctx.driver.uploadUniform(ctx, location, value, slots, components);
	}


	cnvgl.getUniformLocation = function(program, name) {
		var ctx, program_obj, u;

		ctx = cnvgl.getCurrentContext();
		program_obj = ctx.shared.shaderObjects[program];

		if (!program_obj) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		if (!program_obj instanceof cnvgl.program) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}	

			if (!program_obj.link_status) {
			cnvgl.throw_error(cnvgl.INVALID_OPERATION, ctx);
			return;
		}

			if (u = program_obj.uniforms.names[name]) {
			return u.location;
		}

			return -1;
	};


	cnvgl.uniform1f = function(location, v0) {
		cnvgl_uniform(location, [v0], 1, 1);
	};


	cnvgl.uniform1i = function(location, v0) {
		cnvgl_uniform(location, [v0], 1, 1);
	};


	cnvgl.uniform1ui = function(location, v0) {
		cnvgl_uniform(location, [v0], 1, 1);
	};


	cnvgl.uniform2f = function(location, v0, v1) {
		cnvgl_uniform(location, [v0, v1], 1, 2);
	};


	cnvgl.uniform2i = function(location, v0, v1) {
		cnvgl_uniform(location, [v0, v1], 1, 2);
	};


	cnvgl.uniform2ui = function(location, v0, v1) {
		cnvgl_uniform(location, [v0, v1], 1, 2);
	};


	cnvgl.uniform3f = function(location, v0, v1, v2) {
		cnvgl_uniform(location, [v0, v1, v2], 1, 3);
	};


	cnvgl.uniform3i = function(location, v0, v1, v2) {
		cnvgl_uniform(location, [v0, v1, v2], 1, 3);
	};


	cnvgl.uniform3ui = function(location, v0, v1, v2) {
		cnvgl_uniform(location, [v0, v1, v2], 1, 3);
	};


	cnvgl.uniform4f = function(location, v0, v1, v2, v4) {
		cnvgl_uniform(location, [v0, v1, v2, v3], 1, 4);
	};


	cnvgl.uniform4i = function(location, v0, v1, v2, v3) {
		cnvgl_uniform(location, [v0, v1, v2, v3], 1, 4);
	};


	cnvgl.uniform4ui = function(location, v0, v1, v2, v3) {
		cnvgl_uniform(location, [v0, v1, v2, v3], 1, 4);
	};


	cnvgl.uniform1fv = function(location, count, value) {
		var i;
		for (i = 0; i < count; i++) {
			cnvgl_uniform(i + location, [value[i]], 1, 1);
		}
	};


	cnvgl.uniform1iv = function(location, count, value) {
		var i;
		for (i = 0; i < count; i++) {
			cnvgl_uniform(i + location, [value[i]], 1, 1);
		}
	};


	cnvgl.uniform1uiv = function(location, count, value) {
		var i;
		for (i = 0; i < count; i++) {
			cnvgl_uniform(i + location, [value[i]], 1, 1);
		}
	};


	cnvgl.uniform2fv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 2 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1]], 1, 2);
		}
	};


	cnvgl.uniform2iv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 2 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1]], 1, 2);
		}
	};


	cnvgl.uniform2uiv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 2 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1]], 1, 2);
		}
	};


	cnvgl.uniform3fv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 3 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1], value[v + 2]], 1, 3);
		}
	};


	cnvgl.uniform3iv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 3 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1], value[v + 2]], 1, 3);
		}
	};


	cnvgl.uniform3uiv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 3 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1], value[v + 2]], 1, 3);
		}
	};


	cnvgl.uniform3fv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 4 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1], value[v + 2]], 1, 3);
		}
	};


	cnvgl.uniform4iv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 4 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1], value[v + 2], value[v + 3]], 1, 4);
		}
	};


	cnvgl.uniform4uiv = function(location, count, value) {
		var i, v;
		for (i = 0; i < count; i++) {
			v = 4 * i;
			cnvgl_uniform(i + location, [value[v], value[v + 1], value[v + 2], value[v + 3]], 1, 4);
		}
	};


	cnvgl.uniformMatrix2fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 4 * i;
			l = i * 2 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,     [value[v    ], value[v + 1]], 1, 2);
				cnvgl_uniform(l + 1, [value[v + 2], value[v + 3]], 1, 2);
			} else {
				cnvgl_uniform(l,     [value[v    ], value[v + 2]], 1, 2);
				cnvgl_uniform(l + 1, [value[v + 1], value[v + 3]], 1, 2);
			}
		}
	};


	cnvgl.uniformMatrix2x3fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 6 * i;
			l = i * 3 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,     [value[v    ], value[v + 1]], 1, 2);
				cnvgl_uniform(l + 1, [value[v + 2], value[v + 3]], 1, 2);
				cnvgl_uniform(l + 2, [value[v + 4], value[v + 5]], 1, 2);
			} else {
				cnvgl_uniform(l,     [value[v    ], value[v + 3]], 1, 2);
				cnvgl_uniform(l + 1, [value[v + 1], value[v + 4]], 1, 2);
				cnvgl_uniform(l + 2, [value[v + 2], value[v + 5]], 1, 2);				
			}
		}
	};


	cnvgl.uniformMatrix2x4fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 8 * i;
			l = i * 4 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,     [value[v    ], value[v + 1]], 1, 2);
				cnvgl_uniform(l + 1, [value[v + 2], value[v + 3]], 1, 2);
				cnvgl_uniform(l + 2, [value[v + 4], value[v + 5]], 1, 2);
				cnvgl_uniform(l + 3, [value[v + 6], value[v + 7]], 1, 2);
			} else {
				cnvgl_uniform(l,     [value[v    ], value[v + 4]], 1, 2);
				cnvgl_uniform(l + 1, [value[v + 1], value[v + 5]], 1, 2);
				cnvgl_uniform(l + 2, [value[v + 2], value[v + 6]], 1, 2);				
				cnvgl_uniform(l + 3, [value[v + 3], value[v + 7]], 1, 2);
			}
		}
	};


	cnvgl.uniformMatrix3x2fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 6 * i;
			l = i * 2 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,     [value[v    ], value[v + 1], value[v + 2]], 1, 3);
				cnvgl_uniform(l + 1, [value[v + 3], value[v + 4], value[v + 5]], 1, 3);
			} else {
				cnvgl_uniform(l,     [value[v    ], value[v + 2], value[v + 4]], 1, 3);
				cnvgl_uniform(l + 1, [value[v + 1], value[v + 3], value[v + 5]], 1, 3);
			}
		}
	};


	cnvgl.uniformMatrix3fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 9 * i;
			l = i * 3 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,
							  [value[v    ], value[v + 1], value[v + 2],
							   value[v + 3], value[v + 4], value[v + 5],
							   value[v + 6], value[v + 7], value[v + 8]],
							  3, 3);
			} else {
				cnvgl_uniform(l,
							  [value[v    ], value[v + 3], value[v + 6],
							   value[v + 1], value[v + 4], value[v + 7],
							   value[v + 2], value[v + 5], value[v + 8]],
							  3, 3);
			}
		}
	};


	cnvgl.uniformMatrix3x4fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 12 * i;
			l = i * 4 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,     [value[v    ], value[v + 1], value[v + 2]], 1, 3);
				cnvgl_uniform(l + 1, [value[v + 3], value[v + 4], value[v + 5]], 1, 3);
				cnvgl_uniform(l + 2, [value[v + 6], value[v + 7], value[v + 8]], 1, 3);
				cnvgl_uniform(l + 3, [value[v + 9], value[v +10], value[v +11]], 1, 3);
			} else {
				cnvgl_uniform(l,     [value[v    ], value[v + 4], value[v + 8]], 1, 3);
				cnvgl_uniform(l + 1, [value[v + 1], value[v + 5], value[v + 9]], 1, 3);
				cnvgl_uniform(l + 2, [value[v + 2], value[v + 6], value[v +10]], 1, 3);
				cnvgl_uniform(l + 3, [value[v + 3], value[v + 7], value[v +11]], 1, 3);
			}
		}
	};


	cnvgl.uniformMatrix4x2fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 8 * i;
			l = i * 2 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,     [value[v    ], value[v + 1], value[v + 2], value[v + 3]], 1, 4);
				cnvgl_uniform(l + 1, [value[v + 4], value[v + 5], value[v + 6], value[v + 7]], 1, 4);
			} else {
				cnvgl_uniform(l,     [value[v    ], value[v + 2], value[v + 4], value[v + 6]], 1, 4);
				cnvgl_uniform(l + 1, [value[v + 1], value[v + 3], value[v + 5], value[v + 7]], 1, 4);
			}
		}
	};


	cnvgl.uniformMatrix4x3fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 12 * i;
			l = i * 3 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,
					[value[v    ], value[v + 1], value[v + 2], value[v + 3],
					 value[v + 4], value[v + 5], value[v + 6], value[v + 7],
					 value[v + 8], value[v + 9], value[v +10], value[v +11]],
					3, 4);
			} else {
				cnvgl_uniform(l,     [value[v    ], value[v + 3], value[v + 6], value[v + 9]], 1, 4);
				cnvgl_uniform(l + 1, [value[v + 1], value[v + 4], value[v + 7], value[v +10]], 1, 4);
				cnvgl_uniform(l + 2, [value[v + 2], value[v + 5], value[v + 8], value[v +11]], 1, 4);
			}
		}
	};


	cnvgl.uniformMatrix4fv = function(location, count, transpose, value) {
		var i, v, l;
		for (i = 0; i < count; i++) {
			v = 16 * i;
			l = i * 4 + location;
			if (transpose != cnvgl.TRUE) {
				cnvgl_uniform(l,
							  [value[v    ], value[v + 1], value[v + 2], value[v + 3],
							   value[v + 4], value[v + 5], value[v + 6], value[v + 7],
							   value[v + 8], value[v + 9], value[v +10], value[v +11],
							   value[v +12], value[v +13], value[v +14], value[v +15]],
							  4, 4);
			} else {
				cnvgl_uniform(l,
							  [value[v    ], value[v + 4], value[v + 8], value[v +12],
							   value[v + 1], value[v + 5], value[v + 9], value[v +13],
							   value[v + 2], value[v + 6], value[v +10], value[v +14],
							   value[v + 3], value[v + 7], value[v +11], value[v +15]],
							  4, 4);
			}
		}
	};


}(cnvgl));





(function(cnvgl) {


	function cnvgl_vertexAttrib(index, v0, v1, v2, v3) {
		var ctx, buffer_obj, vtx_attrib_obj;

		ctx = cnvgl.getCurrentContext();

		if (index > cnvgl.MAX_VERTEX_ATTRIBS) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		vtx_attrib_obj = ctx.array.arrayObj.vertexAttrib[index];
		vtx_attrib_obj.size = v.length;
		vtx_attrib_obj.type = cnvgl.FLOAT;
		vtx_attrib_obj.normalized = false;
		vtx_attrib_obj.stride = 0;
		vtx_attrib_obj.pointer = 0;
		vtx_attrib_obj.buffer_obj = null;
		vtx_attrib_obj.value = [v0, v1, v2, v3];
	}


	cnvgl.enableVertexAttribArray = function(index) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		if (index >= ctx.const.vertexProgram.maxAttribs || index < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

		ctx.array.arrayObj.vertexAttrib[index].enabled = cnvgl.TRUE;

		ctx.driver.enableVertexAttribArray(ctx, index);
	};


	cnvgl.disableVertexAttribArray = function(index) {
		var ctx;

				ctx = cnvgl.getCurrentContext();

		if (index >= ctx.const.vertexProgram.maxAttribs || index < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			ctx.array.arrayObj.vertexAttrib[index].enabled = cnvgl.FALSE;

		ctx.driver.disableVertexAttribArray(ctx, index);
	};


	cnvgl.vertexAttrib1d = function(index, v0) {
		cnvgl_vertexAttrib(index, [v0]);
	};


	cnvgl.vertexAttrib1f = function(index, v0) {
		cnvgl_vertexAttrib(index, v0, 0, 0, 1);
	};


	cnvgl.vertexAttrib1s = function(index, v0) {
		cnvgl_vertexAttrib(index, v0, 0, 0, 1);
	};


	cnvgl.vertexAttrib2d = function(index, v0, v1) {
		cnvgl_vertexAttrib(index, v0, v1, 0, 1);
	};


	cnvgl.vertexAttrib2f = function(index, v0, v1) {
		cnvgl_vertexAttrib(index, v0, v1, 0, 1);
	};


	cnvgl.vertexAttrib2s = function(index, v0, v1) {
		cnvgl_vertexAttrib(index, v0, v1, 0, 1);
	};


	cnvgl.vertexAttrib3d = function(index, v0, v1, v2) {
		cnvgl_vertexAttrib(index, v0, v1, v2, 1);
	};


	cnvgl.vertexAttrib3f = function(index, v0, v1, v2) {
		cnvgl_vertexAttrib(index, v0, v1, v2, 1);
	};


	cnvgl.vertexAttrib3s = function(index, v0, v1, v2) {
		cnvgl_vertexAttrib(index, v0, v1, v2, 1);
	};


	cnvgl.vertexAttrib4d = function(index, v0, v1, v2, v3) {
		cnvgl_vertexAttrib(index, v0, v1, v2, v3);
	};


	cnvgl.vertexAttrib4f = function(index, v0, v1, v2, v3) {
		cnvgl_vertexAttrib(index, v0, v1, v2, v3);
	};


	cnvgl.vertexAttrib4s = function(index, v0, v1, v2, v3) {
		cnvgl_vertexAttrib(index, v0, v1, v2, v3);
	};



	cnvgl.vertexAttribPointer = function(index, size, type, normalized, stride, pointer) {
		var ctx, buffer_obj, vtx_attrib_obj;

		ctx = cnvgl.getCurrentContext();

		if (index > cnvgl.MAX_VERTEX_ATTRIBS) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;			
		}

			if (size < 1 || size > 4) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}


				if ([cnvgl.BYTE, cnvgl.UNSIGNED_BYTE, cnvgl.SHORT, cnvgl.UNSIGNED_SHORT, cnvgl.INT, cnvgl.UNSIGNED_INT, cnvgl.FLOAT, cnvgl.DOUBLE].indexOf(type) == -1) {
			cnvgl.throw_error(cnvgl.INVALID_ENUM, ctx);
			return;			
		}

		if (stride < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;			
		}

		buffer_obj = ctx.array.arrayBufferObj;

				pointer /= buffer_obj.bpe;

		vtx_attrib_obj = ctx.array.arrayObj.vertexAttrib[index];
		vtx_attrib_obj.size = size;
		vtx_attrib_obj.type = type;
		vtx_attrib_obj.normalized = normalized;
		vtx_attrib_obj.stride = stride;
		vtx_attrib_obj.pointer = pointer;
		vtx_attrib_obj.buffer_obj = buffer_obj;
	};


}(cnvgl));




(function(cnvgl) {


	cnvgl.depthRange = function(nearVal, farVal) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		ctx.viewport.near = nearVal;
		ctx.viewport.far = farVal;

		ctx.driver.depthRange(ctx, nearVal, farVal);
	};


	cnvgl.viewport = function(x, y, width, height) {
		var ctx;
		ctx = cnvgl.getCurrentContext();

		if (width < 0 || height < 0) {
			cnvgl.throw_error(cnvgl.INVALID_VALUE, ctx);
			return;
		}

			ctx.viewport.x = x;
		ctx.viewport.y = y;
		ctx.viewport.w = width;
		ctx.viewport.h = height;

		ctx.driver.viewport(ctx, x, y, width, height);
	};


}(cnvgl));




(function(cnvgl) {

	cnvgl.malloc = function(size, stride, format) {
		var block, i;

		format = format && format.native ? format : Array;
		stride = stride || 1;
		size = Math.ceil(size / stride);

		block = {
			size : size * stride,
			stride : stride
		};

		if (stride > 1) {
			block.data = new Array(size);
			for (i = 0; i < size; i++) {
				block.data[i] = new format(stride);
			}
		} else {
			block = new format(size);
		}

		return block;
	};

	cnvgl.memcpy = function(dest, di, source, size, si) {
		var data, srci, dc, ds;

		ds = dest.stride || 1;
		data = dest.data || dest;

		si = si || 0;
		dc = di % ds;
		di = (di - dc) / ds;

		if (ds == 1) {
			for (srci = si; srci < si + size; srci++) {
				data[di++] = source[srci];
			}
		} else {
			for (srci = si; srci < si + size; srci++) {
				data[di][dc++] = source[srci];
				if (dc == ds) {
					dc = 0;
					di++;
				}
			}
		}
	};

	cnvgl.memset = function(dest, di, value, size) {
		var i, dc, stride, data;

		data = dest.data || dest;
		stride = dest.stride || 1;
		size = size || data.length * stride;

		dc = di % stride;
		di = (di - dc) / stride;

		if (stride == 1) {
			for (i = 0; i < size; i++) {
				data[di++] = value;
			}
		} else {
			for (i = 0; i < size; i++) {
				data[di][dc++] = value;
				if (dc == stride) {
					dc = 0;
					di++;
				}
			}
		}
	};

	cnvgl.memseta = function(dest, di, src, n) {
		var i, dc, si, ss, ds, data;

		data = dest.data || dest;
		ds = dest.stride || 1;
		n = n || data.length;

		dc = di % ds;
		di = (di - dc) / ds;

		si = 0;
		ss = src.length;

		if (ds == 1) {
			for (i = 0; i < n; i++) {
				data[di++] = src[si++];
				if (si == ss) {
					si = 0;
				}
			}
		} else {
			for (i = 0; i < n; i++) {
				data[di][dc++] = src[si++];
				if (dc == ds) {
					dc = 0;
					di++;
				}
				if (si == ss) {
					si = 0;
				}
			}
		}
	};

}(cnvgl));








function DriverCnvGL(canvas, config) {
	var colorBuffer,
	    depthBuffer,
		stencilBuffer
		;

		cWebGL.Driver.Initializer.apply(this);

		this.Driver(canvas, config);

	if (this._context2d = canvas.getContext('2d', null, true)) {

		this.ready = true;
		this.width = canvas.width;
		this.height = canvas.height;

		this.canvasBuffer = this._context2d.createImageData(this.width, this.height);

		if (!GPU.renderer) {
			GPU.renderer = new cnvgl_renderer();
		}
		this.queue = new GPU.CommandQueue(this);
		this._context = GPU.createContext();

		colorBuffer = new GPU.ColorBuffer(this.width, this.height, 4);
		colorBuffer.data = this.canvasBuffer.data;
		this._context.colorBuffer = colorBuffer;

		depthBuffer = new GPU.DepthBuffer(this.width, this.height);
		this._context.depthBuffer = depthBuffer;

		stencilBuffer = new GPU.ColorBuffer(this.width, this.height, 1);
		this._context.stencilBuffer = stencilBuffer;

		DriverCnvGL.animationFrameFunc = DriverCnvGL.requestAnimationFrame;
	}
}

util.inherits(DriverCnvGL, cWebGL.Driver);

cWebGL.drivers.cnvGL = DriverCnvGL;



DriverCnvGL.test = function() {
	return true;
};

DriverCnvGL.animationFrameQueue = [];

DriverCnvGL.animationFrameFunc = true;

DriverCnvGL.requestAnimationFrameNative = null;
DriverCnvGL.requestAnimationFrame = function(func, el) {
	DriverCnvGL.animationFrameQueue.push(func);
};
DriverCnvGL.requestAnimationFrameWrapper = function(func, el) {
	DriverCnvGL.animationFrameFunc.call(window, func, el);
};

DriverCnvGL.setupRequestAnimationFrame = function() {
	DriverCnvGL.requestAnimationFrameNative =
		window.requestAnimationFrame ||
		window.webkitRequestAnimationFrame ||
		window.mozRequestAnimationFrame ||
		window.oRequestAnimationFrame ||
		window.msRequestAnimationFrame ||
		function(func, el) {
			window.setTimeout(el, 1000 / 60);
		};

	DriverCnvGL.animationFrameFunc = DriverCnvGL.requestAnimationFrameNative;
	window.requestAnimationFrame = DriverCnvGL.requestAnimationFrameWrapper;
};

DriverCnvGL.frameComplete = function() {
	var list;
	list = DriverCnvGL.animationFrameQueue;
	while (list.length > 0) {
		window.setTimeout(list.shift(), 0);
	}
};

DriverCnvGL.setupRequestAnimationFrame();


var proto = DriverCnvGL.prototype;


proto.command = function() {
	var args;
	args = [].slice.call(arguments, 0);
	args.unshift(this._context);
	this.queue.enqueue(args);
};


proto.clear = function(ctx, color, depth, stencil, mask) {

	if (mask && cnvgl.COLOR_BUFFER_BIT) {
		this.command('clearColorBuffer', color);
	}

	if (mask && cnvgl.DEPTH_BUFFER_BIT) {
		this.command('clearDepthBuffer', depth);
	}

		if (mask && cnvgl.STENCIL_BUFFER_BIT) {
		this.command('clearStencilBuffer', stencil);
	}
};

proto.colorMask = function(ctx, r, g, b, a) {
	this.command('set', 'colorMask', [r, g, b, a]);
};

proto.compileShader = function(ctx, shader, source, type) {

	var options, state;

		options = {};

	switch (type) {
		case cnvgl.FRAGMENT_SHADER:
			options.target = glsl.target.fragment;
			break;
		case cnvgl.VERTEX_SHADER:
			options.target = glsl.target.vertex;
			break;
	}

	state = glsl.compile(source, options);

	this.compileStatus = state.status;
	this.compileLog = state.errors.join("\n");

	if (this.compileStatus) {
		shader.out = state;
	}
};

proto.cullFace = function(ctx, mode) {
	this.command('set', 'cullFaceMode', mode);
};

proto.createProgram = function() {
	var program;
	program = new cWebGL.Driver.Program();
	return program;
};

proto.createShader = function(ctx, type) {
	return {};
};

proto.depthRange = function(ctx, n, f) {
	this.command('set', 'viewportN', n);
	this.command('set', 'viewportF', f);
};

proto.depthFunc = function(ctx, f) {
	var func, fns;

	fns = GPU.constants.fragment;

	switch (f) {

		case cnvgl.NEVER:
			func = fns.depthFunc_never;
			break;

		case cnvgl.ALWAYS:
			func = fns.depthFunc_always;
			break;

		case cnvgl.LESS:
			func = fns.depthFunc_less;
			break;

				case cnvgl.LEQUAL:
			func = fns.depthFunc_ltEqual;
			break;

				case cnvgl.EQUAL:
			func = fns.depthFunc_equal;
			break;

				case cnvgl.GREATER:
			func = fns.depthFunc_greater;
			break;

		case cnvgl.GEQUAL:
			func = fns.depthFunc_gtEqual;
			break;

				case cnvgl.NOTEQUAL:
			func = fns.depthFunc_nEqual;
			break;

		default:
			return;
	}		

	this.command('set', 'depthFunc', func);
};

proto.depthMask = function(ctx, mask) {
	this.command('set', 'depthMask', mask);
};

proto.disableVertexAttribArray = function(ctx, index) {

};

proto.drawArrays = function(ctx, mode, first, count) {
	this.command('drawPrimitives', mode, first, count);
};

proto.drawElements = function(ctx, mode, first, count, type) {
	var buffer;
	buffer = ctx.array.elementArrayBufferObj.data;
	this.command('drawIndexedPrimitives', mode, buffer, first, count, type);
};

proto.enable = function(ctx, flag, v) {
	switch (flag) {
		case cnvgl.BLEND:
			this.command('set', 'blendEnabled', v);
			break;
		case cnvgl.CULL_FACE:
			this.command('set', 'cullFlag', v);
			break;
		case cnvgl.DEPTH_TEST:
			this.command('set', 'depthTest', v);
			break;
		case cnvgl.DITHER:
			break;
		case cnvgl.SCISSOR_TEST:
			this.command('set', 'scissorTest', v);
			break;
		default:
			console.log(flag);
	}
};

proto.enableVertexAttribArray = function(ctx, index) {

};

proto.flush = function(ctx, mode) {
};

proto.frontFace = function(ctx, mode) {
	this.command('set', 'cullFrontFace', mode);
};

proto.link = function(ctx, program, shaders) {
	var i, code, prgm;

	prgm = glsl.createProgram('js', {
		max_vertex_attribute_vectors : GPU.capabilities.vertex_attribute_vectors,
		max_vertex_uniform_vectors   : GPU.capabilities.vertex_uniform_vectors,
		max_varying_vectors          : GPU.capabilities.varying_vectors,
		max_fragment_uniform_vectors : GPU.capabilities.fragment_uniform_vectors
	});

	for (i = 0; i < shaders.length; i++) {
		code = shaders[i].out.getIR();
		prgm.addObjectCode(code, shaders[i].out.options.target);
	}

	if (prgm.errors.length) {
		this.linkStatus = false;
		this.linkLog = prgm.errors.join("\n");
		return;
	}

	this.linkStatus = true;
	this.linkLog = "";

	prgm.setTexFunction2D(GPU.getTexFunction2D());
	prgm.build();


	program.exec = prgm;

	program.attributes = prgm.symbols.attribute;
	program.uniforms = prgm.symbols.uniform;
	program.varying = prgm.symbols.varying;

	var varying;
	varying = new Array(GPU.capabilities.varying_vectors);

	for (i in program.varying) {
		for (j = 0; j < program.varying[i].slots; j++) {
			varying[program.varying[i].pos + j] = program.varying[i].components;
		}
	}

	for (i = 0; i < varying.length; i++) {
		this.command('setArray', 'activeVarying', i, varying[i] || 0);
	}
};

proto.polygonOffset = function(ctx, factor, units) {
	this.command('set', 'polygonOffsetFactor', factor);
	this.command('set', 'polygonOffsetUnits', units);
};

proto.present = function() {
	this._context2d.putImageData(this.canvasBuffer, 0, 0);
	DriverCnvGL.frameComplete();
};

proto.sampleCoverage = function(ctx, value, invert) {
	this.command('set', 'mulitsampleCoverageValue', value);
	this.command('set', 'mulitsampleCoverageInvert', invert);
};

proto.stencilFunc = function(ctx, func, ref, mask) {
	this.command('set', 'stencilFuncFront', func);
	this.command('set', 'stencilFuncBack', func);
	this.command('set', 'stencilRefFront', ref);
	this.command('set', 'stencilRefBack', ref);
	this.command('set', 'stencilValueMaskFront', mask);
	this.command('set', 'stencilValueMaskBack', mask);
};

proto.stencilOp = function(ctx, sfail, dpfail, dppass) {
	this.command('set', 'stencilFailFuncBack', sfail);
	this.command('set', 'stencilFailFuncFront', sfail);
	this.command('set', 'stencilZFailFuncBack', dpfail);
	this.command('set', 'stencilZFailFuncFront', dpfail);
	this.command('set', 'stencilZPassFuncBack', dppass);
	this.command('set', 'stencilZPassFuncFront', dppass);
};

proto.stencilMask = function(ctx, mask) {
	this.command('set', 'stencilWriteMaskFront', mask);
	this.command('set', 'stencilWriteMaskBack', mask);
};

proto.scissor = function(ctx, x, y, width, height) {
	this.command('set', 'scissorX', x);
	this.command('set', 'scissorY', y);
	this.command('set', 'scissorWidth', width);
	this.command('set', 'scissorHeight', height);
};

proto.uploadAttributes = function(ctx, location, size, stride, pointer, data) {
	this.command('uploadAttributes', location, size, stride, pointer, data);
};

proto.uploadUniform = function(ctx, location, data, slots, components) {
	this.command('uploadUniforms', location, data, slots, components);
};

proto.useProgram = function(ctx, program) {
	this.command('uploadProgram', program.exec);
};


proto.viewport = function(ctx, x, y, w, h) {
	this.command('set', 'viewportX', x);
	this.command('set', 'viewportY', y);
	this.command('set', 'viewportW', w);
	this.command('set', 'viewportH', h);
};



proto.blendColor = function(ctx, r, g, b, a) {
	this.command('set', 'blendColor', [r, g, b, a]);
};

proto.blendFunc = function(ctx, sfactor, dfactor) {
	this.command('blendFunc', getGPUBlendFunction(sfactor), getGPUBlendFunction(dfactor));	
};

proto.blendEquationSeparate = function(ctx, rgb, a) {
	this.command('blendEqs', getGPUBlendEquation(rgb), getGPUBlendEquation(a));
};


function getGPUBlendEquation(mode) {

	switch (mode) {

		case cnvgl.FUNC_ADD:
			return GPU.constants.fnBlendEqAdd;

		case cnvgl.FUNC_SUBTRACT:
			return GPU.constants.fnBlendEqSub;

				case cnvgl.FUNC_REVERSE_SUBTRACT:
			return GPU.constants.fnBlendEqRevSub;
	}

	throw new Error("cnvGL.driver: Invalid equation");
}

function getGPUBlendFunction(fn) {

	switch (fn) {

		case cnvgl.ONE:
			return GPU.constants.fnBlendFnOne;

		case cnvgl.ZERO:
			return GPU.constants.fnBlendFnZero;

				case cnvgl.SRC_ALPHA:
			return GPU.constants.fnBlendFnSrcAlpha;

		case cnvgl.ONE_MINUS_SRC_ALPHA:
			return GPU.constants.fnBlendFnOneMinusSrcAlpha;

		case cnvgl.DST_ALPHA:
			return GPU.constants.fnBlendFnDestAlpha;

		case cnvgl.ONE_MINUS_DST_ALPHA:
			return GPU.constants.fnBlendFnOneMinusDestAlpha;
	}

	throw new Error("cnvGL.driver: Invalid function");
}





function getGPUTextureTarget(gl_target) {
	var cnst = GPU.constants.texture.targets;

	switch (gl_target) {

		case cnvgl.TEXTURE_2D:
			return cnst.texture_2D;
	}

	throw new Error("cnvGL.driver: Invalid texture target");
}

function getGPUPixelFormat(format) {
	var cnst = GPU.constants.texture.image.format;

	switch (format) {

				case cnvgl.RGB:
			return cnst.rgb;

		case cnvgl.RGBA:
			return cnst.rgba;
	}

	throw new Error("cnvGL.driver: Invalid pixel format");
}

function getGPUTextureFilter(filter) {
	var cnst = GPU.constants.texture.func;

	switch (filter) {

				case cnvgl.LINEAR:
			return cnst.linear;

		case cnvgl.NEAREST:
			return cnst.nearest;

				case cnvgl.LINEAR_MIPMAP_NEAREST:
			return cnst.linear_mipmap_nearest;

		case cnvgl.NEAREST_MIPMAP_LINEAR:
			return cnst.nearest_mipmap_linear;
	}

	throw new Error("cnvGL.driver: Invalid texture filter");
}




proto.newTextureObject = function() {
	return new GPU.TextureObject();
};


proto.bindTexture = function(ctx, target, tex_obj) {
	this.command('bindTexture', getGPUTextureTarget(target), tex_obj.driverObj, ctx.texture.currentUnit);
};


proto.texParameter = function(ctx, target, tex_obj, pname, param) {
	var cmd, parm;

	switch (pname) {

		case cnvgl.TEXTURE_MIN_FILTER:
			cmd = 'TextureMinFilter';
			parm = getGPUTextureFilter(param);
			break;

		case cnvgl.TEXTURE_MAG_FILTER:
			cmd = 'TextureMagFilter';
			parm = getGPUTextureFilter(param);
			break;

		default:
			return;
	}

	this.command('texParameter' + cmd, tex_obj.driverObj, parm);
};

proto.texImage2D = function(ctx, target, level, internalFormat, width, height, depth, border, format, type, data, unpack, texture_obj, texture_image) {

	this.command('texImage2D',
		texture_obj.driverObj,
		level,
		internalFormat,
		width,
		height,
		getGPUPixelFormat(format),
		type,
		data
		);		
};



proto.renderTexture = function(ctx, fb_obj, tex_obj, textarget, level, offset) {
	this.command('renderTexture', fb_obj, tex_obj, textarget, level, offset);
};



