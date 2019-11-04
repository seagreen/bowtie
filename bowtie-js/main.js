"use strict";

function $arrayToListBuiltin(xs) {
  const reducer = (accumulator, currentValue) => [
    "Cons",
    currentValue,
    accumulator
  ];
  return xs.reduceRight(reducer, ["Nil"]);
}

function $unicodeListizeBuiltin(t) {
  let xs = t.split("").map(c => c.codePointAt());
  return ["Unicode", $arrayToListBuiltin(xs)];
}

function $compareBuiltin(a, b) {
  if (a > b) {
    return ["GreaterThan"];
  } else if (b > a) {
    return ["LessThan"];
  } else {
    return ["Equal"];
  }
}

const _Cons = _arg1 => _arg2 => ["Cons", _arg1, _arg2];
const _Destroyed = ["Destroyed"];
const _Done = ["Done"];
const _Equal = ["Equal"];
const _False = ["False"];
const _Flying = ["Flying"];
const _GreaterThan = ["GreaterThan"];
const _Just = _arg1 => ["Just", _arg1];
const _KeyDown = _arg1 => ["KeyDown", _arg1];
const _KeyUp = _arg1 => ["KeyUp", _arg1];
const _Landed = ["Landed"];
const _Left = _arg1 => ["Left", _arg1];
const _LessThan = ["LessThan"];
const _Line = _arg1 => _arg2 => ["Line", _arg1, _arg2];
const _Model = _arg1 => _arg2 => _arg3 => _arg4 => _arg5 => [
  "Model",
  _arg1,
  _arg2,
  _arg3,
  _arg4,
  _arg5
];
const _Natural = _arg1 => ["Natural", _arg1];
const _Nil = ["Nil"];
const _Nothing = ["Nothing"];
const _Pair = _arg1 => _arg2 => ["Pair", _arg1, _arg2];
const _Pictures = _arg1 => ["Pictures", _arg1];
const _Point = _arg1 => _arg2 => ["Point", _arg1, _arg2];
const _Right = _arg1 => ["Right", _arg1];
const _Step = _arg1 => _arg2 => ["Step", _arg1, _arg2];
const _Text = _arg1 => ["Text", _arg1];
const _Tick = ["Tick"];
const _Translate = _arg1 => _arg2 => _arg3 => [
  "Translate",
  _arg1,
  _arg2,
  _arg3
];
const _True = ["True"];
const _Unicode = _arg1 => ["Unicode", _arg1];
const _Unit = ["Unit"];
const _wCodepoint = 119;
const _updateStatus = _m => _status =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _a = $1[1];
      const _b = $1[2];
      const _c = $1[3];
      const _d = $1[4];
      const _oldStatus = $1[5];
      return _Model(_a)(_b)(_c)(_d)(_status);
    } else {
      throw "no match";
    }
  })();
const _tail = _xs =>
  (() => {
    const $1 = _xs;
    if ($1[0] === "Cons") {
      const _x = $1[1];
      const _rest = $1[2];
      return _rest;
    } else if ($1[0] === "Nil") {
      return _Nil;
    } else {
      throw "no match";
    }
  })();
const _setThrust = _m => _thrust =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _a = $1[1];
      const _b = $1[2];
      const _c = $1[3];
      const _oldThrust = $1[4];
      const _d = $1[5];
      return _Model(_a)(_b)(_c)(_thrust)(_d);
    } else {
      throw "no match";
    }
  })();
const _panic = _a =>
  (() => {
    throw _a;
  })();
const _listMap = _f => _xs =>
  (() => {
    const $1 = _xs;
    if ($1[0] === "Cons") {
      const _x = $1[1];
      const _rest = $1[2];
      return _Cons(_f(_x))(_listMap(_f)(_rest));
    } else if ($1[0] === "Nil") {
      return _Nil;
    } else {
      throw "no match";
    }
  })();
const _listAppend = _xs => _ys =>
  (() => {
    const $1 = _xs;
    if ($1[0] === "Nil") {
      return _ys;
    } else if ($1[0] === "Cons") {
      const _x = $1[1];
      const _rest = $1[2];
      return _Cons(_x)(_listAppend(_rest)(_ys));
    } else {
      throw "no match";
    }
  })();
const _textAppend = _t1 => _t2 =>
  (() => {
    const $1 = _t1;
    if ($1[0] === "Unicode") {
      const _a = $1[1];
      return (() => {
        const $1 = _t2;
        if ($1[0] === "Unicode") {
          const _b = $1[1];
          return _Unicode(_listAppend(_a)(_b));
        } else {
          throw "no match";
        }
      })();
    } else {
      throw "no match";
    }
  })();
const _line = _xa => _ya => _xb => _yb =>
  _Line(_Point(_xa)(_ya))(_Point(_xb)(_yb));
const _viewThrust = (() => {
  const _topRight = _line(0)(-5)(5)(0);
  return (() => {
    const _topLeft = _line(-5)(0)(0)(-5);
    return (() => {
      const _bottomRight = _line(0)(8)(5)(0);
      return (() => {
        const _bottomLeft = _line(-5)(0)(0)(8);
        return _Pictures(
          _Cons(_topLeft)(
            _Cons(_topRight)(_Cons(_bottomLeft)(_Cons(_bottomRight)(_Nil)))
          )
        );
      })();
    })();
  })();
})();
const _lemLegs = (() => {
  const _rightLeg = _line(5)(5)(12)(12);
  return (() => {
    const _leftLeg = _line(-5)(5)(-12)(12);
    return _Pictures(_Cons(_leftLeg)(_Cons(_rightLeg)(_Nil)));
  })();
})();
const _lemBody = (() => {
  const _topBar = _line(-5)(-5)(5)(-5);
  return (() => {
    const _rightBar = _line(5)(-5)(5)(5);
    return (() => {
      const _leftBar = _line(-5)(-5)(-5)(5);
      return (() => {
        const _centerBar = _line(-5)(0)(5)(0);
        return (() => {
          const _bottomBar = _line(-5)(5)(5)(5);
          return _Pictures(
            _Cons(_topBar)(
              _Cons(_bottomBar)(
                _Cons(_leftBar)(_Cons(_rightBar)(_Cons(_centerBar)(_Nil)))
              )
            )
          );
        })();
      })();
    })();
  })();
})();
const _initialState = _Model(0)(0)(100)(_False)(_Flying);
const _identity = _a => _a;
const _head = _xs =>
  (() => {
    const $1 = _xs;
    if ($1[0] === "Cons") {
      const _x = $1[1];
      const _rest = $1[2];
      return _Just(_x);
    } else if ($1[0] === "Nil") {
      return _Nothing;
    } else {
      throw "no match";
    }
  })();
const _getY = _m =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _y = $1[1];
      const _a = $1[2];
      const _b = $1[3];
      const _c = $1[4];
      const _d = $1[5];
      return _y;
    } else {
      throw "no match";
    }
  })();
const _getVelocity = _m =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _a = $1[1];
      const _velocity = $1[2];
      const _b = $1[3];
      const _c = $1[4];
      const _d = $1[5];
      return _velocity;
    } else {
      throw "no match";
    }
  })();
const _getThrust = _m =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _a = $1[1];
      const _b = $1[2];
      const _c = $1[3];
      const _isThrustOn = $1[4];
      const _d = $1[5];
      return _isThrustOn;
    } else {
      throw "no match";
    }
  })();
const _getStatus = _m =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _a = $1[1];
      const _b = $1[2];
      const _c = $1[3];
      const _d = $1[4];
      const _status = $1[5];
      return _status;
    } else {
      throw "no match";
    }
  })();
const _getFuel = _m =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _a = $1[1];
      const _b = $1[2];
      const _fuel = $1[3];
      const _c = $1[4];
      const _d = $1[5];
      return _fuel;
    } else {
      throw "no match";
    }
  })();
const _emptyPicture = _Pictures(_Nil);
const _lem = _isThrusting =>
  (() => {
    const _thrust = (() => {
      const $1 = _isThrusting;
      if ($1[0] === "False") {
        return _emptyPicture;
      } else if ($1[0] === "True") {
        return _viewThrust;
      } else {
        throw "no match";
      }
    })();
    return _Pictures(
      _Cons(_Translate(0)(15)(_thrust))(_Cons(_lemLegs)(_Cons(_lemBody)(_Nil)))
    );
  })();
const _displayWidth = 1600;
const _surface = _line(0)(800)(_displayWidth)(800);
const _displayHeight = 1000;
const _crashSite = (() => {
  const _d = _line(10)(0)(25)(20);
  return (() => {
    const _c = _line(4)(5)(10)(30);
    return (() => {
      const _b = _line(-4)(5)(-10)(30);
      return (() => {
        const _a = _line(-10)(0)(-25)(20);
        return _Pictures(_Cons(_a)(_Cons(_b)(_Cons(_c)(_Cons(_d)(_Nil)))));
      })();
    })();
  })();
})();
const _showLem = _m =>
  (() => {
    const $1 = _getStatus(_m);
    if ($1[0] === "Flying") {
      return _lem(_getThrust(_m));
    } else if ($1[0] === "Landed") {
      return _lem(_getThrust(_m));
    } else if ($1[0] === "Destroyed") {
      return _crashSite;
    } else {
      throw "no match";
    }
  })();
const _builtin = _panic;
const _compare = _a => _b => $compareBuiltin(_a, _b);
const _equal = _a => _b =>
  (() => {
    const $1 = _compare(_a)(_b);
    if ($1[0] === "LessThan") {
      return _False;
    } else if ($1[0] === "Equal") {
      return _True;
    } else if ($1[0] === "GreaterThan") {
      return _False;
    } else {
      throw "no match";
    }
  })();
const _multiply = _a => _b => _a * _b;
const _multPoint = _p => _n =>
  (() => {
    const $1 = _p;
    if ($1[0] === "Point") {
      const _x = $1[1];
      const _y = $1[2];
      return _Point(_multiply(_x)(_n))(_multiply(_y)(_n));
    } else {
      throw "no match";
    }
  })();
const _scale = _n => _pic =>
  (() => {
    const $1 = _pic;
    if ($1[0] === "Line") {
      const _p1 = $1[1];
      const _p2 = $1[2];
      return _Line(_multPoint(_p1)(_n))(_multPoint(_p2)(_n));
    } else if ($1[0] === "Text") {
      const _t = $1[1];
      return _pic;
    } else if ($1[0] === "Translate") {
      const _x = $1[1];
      const _y = $1[2];
      const _p = $1[3];
      return _pic;
    } else if ($1[0] === "Pictures") {
      const _pics = $1[1];
      return _Pictures(_listMap(_scale(_n))(_pics));
    } else {
      throw "no match";
    }
  })();
const _negate = _n => _multiply(_n)(-1);
const _plus = _a => _b => _a + _b;
const _bumpStatus = _m =>
  (() => {
    const $1 = _compare(_getY(_m))(_plus(800)(-15));
    if ($1[0] === "LessThan") {
      return _m;
    } else if ($1[0] === "Equal") {
      return _m;
    } else if ($1[0] === "GreaterThan") {
      return (() => {
        const _modelB = (() => {
          const $1 = _compare(_getVelocity(_m))(-10);
          if ($1[0] === "LessThan") {
            return _updateStatus(_m)(_Destroyed);
          } else if ($1[0] === "Equal") {
            return _updateStatus(_m)(_Landed);
          } else if ($1[0] === "GreaterThan") {
            return _updateStatus(_m)(_Landed);
          } else {
            throw "no match";
          }
        })();
        return _setThrust(_modelB)(_False);
      })();
    } else {
      throw "no match";
    }
  })();
const _length = _xs =>
  (() => {
    const $1 = _xs;
    if ($1[0] === "Cons") {
      const _x = $1[1];
      const _rest = $1[2];
      return _plus(1)(_length(_rest));
    } else if ($1[0] === "Nil") {
      return 0;
    } else {
      throw "no match";
    }
  })();
const _updateFuel = _m => _n =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _a = $1[1];
      const _b = $1[2];
      const _fuel = $1[3];
      const _c = $1[4];
      const _d = $1[5];
      return _Model(_a)(_b)(_plus(_n)(_fuel))(_c)(_d);
    } else {
      throw "no match";
    }
  })();
const _updateVelocity = _m => _n =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _a = $1[1];
      const _velocity = $1[2];
      const _b = $1[3];
      const _c = $1[4];
      const _d = $1[5];
      return _Model(_a)(_plus(_velocity)(_n))(_b)(_c)(_d);
    } else {
      throw "no match";
    }
  })();
const _updateY = _m => _n =>
  (() => {
    const $1 = _m;
    if ($1[0] === "Model") {
      const _y = $1[1];
      const _a = $1[2];
      const _b = $1[3];
      const _c = $1[4];
      const _d = $1[5];
      return _Model(_plus(_y)(_n))(_a)(_b)(_c)(_d);
    } else {
      throw "no match";
    }
  })();
const _bumpPosition = _m => _updateY(_m)(_negate(_getVelocity(_m)));
const _update = _input => _m =>
  (() => {
    const $1 = _getStatus(_m);
    if ($1[0] === "Flying") {
      return (() => {
        const $1 = _input;
        if ($1[0] === "Tick") {
          return (() => {
            const _modelB = (() => {
              const $1 = _getThrust(_m);
              if ($1[0] === "True") {
                return (() => {
                  const $1 = _compare(_getFuel(_m))(0);
                  if ($1[0] === "LessThan") {
                    return _updateVelocity(_setThrust(_m)(_False))(-1);
                  } else if ($1[0] === "Equal") {
                    return _updateVelocity(_setThrust(_m)(_False))(-1);
                  } else if ($1[0] === "GreaterThan") {
                    return _updateFuel(_updateVelocity(_m)(1))(-1);
                  } else {
                    throw "no match";
                  }
                })();
              } else if ($1[0] === "False") {
                return _updateVelocity(_m)(-1);
              } else {
                throw "no match";
              }
            })();
            return _bumpStatus(_bumpPosition(_modelB));
          })();
        } else if ($1[0] === "KeyUp") {
          const _c = $1[1];
          return _setThrust(_m)(_False);
        } else if ($1[0] === "KeyDown") {
          const _c = $1[1];
          return _setThrust(_m)(_True);
        } else {
          throw "no match";
        }
      })();
    } else if ($1[0] === "Landed") {
      return _m;
    } else if ($1[0] === "Destroyed") {
      return _m;
    } else {
      throw "no match";
    }
  })();
const _showInt = _a => $unicodeListizeBuiltin(_a.toString());
const _fuelGuage = _n =>
  _Translate(100)(100)(
    _Text(
      _textAppend(
        _Unicode(
          _Cons(70)(
            _Cons(117)(_Cons(101)(_Cons(108)(_Cons(58)(_Cons(32)(_Nil)))))
          )
        )
      )(_showInt(_n))
    )
  );
const _view = _m =>
  _Pictures(
    _Cons(_fuelGuage(_getFuel(_m)))(
      _Cons(_Translate(800)(_getY(_m))(_showLem(_m)))(_Cons(_surface)(_Nil))
    )
  );
const _step = _input => _m =>
  (() => {
    const _newState = _update(_input)(_m);
    return _Step(_view(_newState))(_input => _step(_input)(_newState));
  })();
const _result = _Step(_view(_initialState))(_input =>
  _step(_input)(_initialState)
);
