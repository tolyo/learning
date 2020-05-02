'use strict';

var _ = require('lodash');

function Scope() {
  this.$$watchers = [];
  this.$$lastDirtyWatch = null;
  this.$$asyncQueue = [];
  this.$$applyAsyncQueue = [];
  this.$$applyAsyncId = null;
  this.$$postDigestQueue = [];
  this.$root = this;
  this.$$children = [];
  this.$$phase = null;
  this.$parent = null;
  this.$$listeners = {};
}

function initWatchVal(){}

Scope.prototype.$watch = function (watchFn, listenerFn, valueEq) {
  var self = this;
  var watcher = {
    watchFn:    watchFn,
    listenerFn: listenerFn || function () { },
    valueEq: !!valueEq,
    last: initWatchVal
  };

  self.$$watchers.unshift(watcher);
  self.$root.$$lastDirtyWatch = null;

  return function () {
    var index = self.$$watchers.indexOf(watcher);
    if (index >= 0) {
      self.$$watchers.splice(index, 1);
      self.$root.$$lastDirtyWatch = null;
    }
  };
};

Scope.prototype.$watchGroup = function(watchFns, listenerFn) {
  var self = this;
  var oldValues = new Array(watchFns.length);
  var newValues = new Array(watchFns.length);
  var changeReactionScheduled = false;
  var firstRun = true;


  if (watchFns.length === 0) {
    var shouldCall = true;
    self.$evalAsync(function() {
      if (shouldCall) {
        listenerFn(newValues, newValues, self);
      }
    });
    return function () {
      shouldCall = false;
    };
  }

  function watchGroupListener () {
    if (firstRun) {
      firstRun = false;
      listenerFn(newValues, newValues, self);
    } else {
      listenerFn(newValues, oldValues, self);
    }
    changeReactionScheduled = false;
  }

  var destroyFunctions = _.map(watchFns, function (watchFn, i) {
    return self.$watch(watchFn, function (newValue, oldValue) {
      newValues[i] = newValue;
      oldValues[i] = oldValue;
      if (!changeReactionScheduled) {
        changeReactionScheduled = true;
        self.$evalAsync(watchGroupListener);
      }
    });
  });

  return function () {
    _.forEach(destroyFunctions, function (destroyFunction) {
      destroyFunction();
    });
  };
};

Scope.prototype.$digest = function() {
  var ttl = 10;
  var dirty;
  this.$root.$$lastDirtyWatch = null;
  this.$beginPhase("$digest");

  if (this.$root.$$applyAsyncId) {
    clearTimeout(this.$root.$$applyAsyncId);
    this.$$flushApplyAsync();
  }

  do {
    while (this.$$asyncQueue.length) {
      try {
        var asyncTask = this.$$asyncQueue.shift();
        asyncTask.scope.$eval(asyncTask.expression);
      } catch (e) {
        console.error(e);
      }
    }
    dirty = this.$$digestOnce();
    if ((dirty || this.$$asyncQueue.length) && !(ttl--)) {
      throw "10 digest iterations reached";
    }
  } while (dirty || this.$$asyncQueue.length);
  this.$clearPhase();

  while (this.$$postDigestQueue.length) {
    try {
      this.$$postDigestQueue.shift()();
    } catch (e) {
      console.error(e);
    }
  }
};

Scope.prototype.$$digestOnce = function () {
  var dirty;
  var continueLoop = true;
  var self = this;
  this.$$everyScope(function(scope) {
    var newValue, oldValue;
    _.forEachRight(scope.$$watchers, function(watcher) {
      try {
        if (watcher) {
          newValue = watcher.watchFn(scope);
          oldValue = watcher.last;
          if (!scope.$$areEqual(newValue, oldValue, watcher.valueEq)) {
            self.$root.$$lastDirtyWatch = watcher;
            watcher.last = (watcher.valueEq ? _.cloneDeep(newValue) : newValue);
            watcher.listenerFn(newValue,
                (oldValue === initWatchVal ? newValue : oldValue),
                scope);
            dirty = true;
          } else if (self.$root.$$lastDirtyWatch === watcher) {
            continueLoop = false;
            return false;
          }
        }
      } catch (e) {
        console.error(e);
      }
    });
    return continueLoop;
  });
  return dirty;
};

Scope.prototype.$$areEqual = function (newValue, oldValue, valueEq) {
  if (valueEq) {
    return _.isEqual(newValue, oldValue);
  } else {
    return newValue === oldValue ||
      (typeof newValue === 'number' &&
       typeof oldValue === 'number' &&
       isNaN(newValue) &&
       isNaN(oldValue)
      );
  }
};

Scope.prototype.$eval = function(expr, locals) {
  return expr(this, locals);
};

Scope.prototype.$apply = function (expr) {
  this.$beginPhase("$apply");
  try {
    this.$eval(expr, null);
  } finally {
    this.$clearPhase();
    this.$root.$digest();
  }
};

Scope.prototype.$evalAsync = function (expr) {
  var self = this;
  if (!self.$$phase && !self.$$asyncQueue.length) {
    setTimeout(function() {
      if (self.$$asyncQueue.length) {
        self.$root.$digest();
      }
    }, 0);
  }
  this.$$asyncQueue.push({scope: this, expression: expr});
};

Scope.prototype.$beginPhase = function (phase) {
  if (this.$$phase) {
    throw this.$$phase + " already in progress";
  }
  this.$$phase = phase;
};


Scope.prototype.$clearPhase = function () {
  this.$$phase = null;
};

Scope.prototype.$applyAsync = function (expr) {
  var self = this;

  self.$$applyAsyncQueue.push(function () {
    self.$eval(expr);
  });

  if (self.$root.$$applyAsyncId === null) {
    self.$root.$$applyAsyncId = setTimeout(function() {
      self.$apply(_.bind(self.$$flushApplyAsync, self));
    }, 0);
  }
};

Scope.prototype.$$flushApplyAsync = function() {
  while (this.$$applyAsyncQueue.length) {
    try {
      this.$$applyAsyncQueue.shift()();
    } catch (e) {
      console.error(e);
    }
  }
  this.$root.$$applyAsyncId = null;
};

Scope.prototype.$$postDigest = function (expr) {
  this.$$postDigestQueue.push(expr);
};

Scope.prototype.$new = function (isolated, parent) {
  var child;
  parent = parent || this;
  if (isolated) {
    child = new Scope();
    child.$root = parent.$root;
    child.$$asyncQueue = parent.$$asyncQueue;
    child.$$postDigestQueue = parent.$$postDigestQueue;
    child.$$applyAsyncQueue = parent.$$applyAsyncQueue;
  } else {
    child = Object.create(this);
  }  
  parent.$$children.push(child);
  child.$$watchers = [];
  child.$$children = [];
  child.$parent = parent;
  child.$$listeners = {};
  return child;
};

Scope.prototype.$$everyScope = function(fn) {
  if (fn(this)) {
    return this.$$children.every(function(child) {
      return child.$$everyScope(fn);
    });
  } else {
    return false;
  }
};

Scope.prototype.$destroy = function() {
  if (this.$parent) {
    this.$parent.$$children = 
      _.filter(this.$parent.$$children, function(i) {
        return i === this;
      });  
  }
  this.$$watchers = null;
};

// Chapter 3
Scope.prototype.$watchCollection = function(watchFn, listenerFn, valueEq) {
  var self = this;
  var newValue;
  var oldValue;
  var oldLength;
  var changeCount = 0;
  var veryOldValue;
  var trackVeryOldValue = (listenerFn.length > 1);
  var firstRun = true;

  var internalWatchFn = function(scope) {
    newValue = watchFn(scope);
    var newLength;

    if (_.isObject(newValue)) {      
      if (isArrayLike(newValue)) {
        if (!_.isArray(oldValue)) {
          changeCount++;
          oldValue = [];
        }

        if (oldValue.length !== newValue.length) {
          changeCount++;
          oldValue.length = newValue.length;  
        }

        if (_.forEach(newValue, function(x, i) {
          
          var bothNaN = _.isNaN(x) && _.isNaN(oldValue[i]);
          if (!bothNaN && x !== oldValue[i]) {
            changeCount++;
            oldValue[i] = x;
          }    
        }));

      } else {
        if (!_.isObject(oldValue) || isArrayLike(oldValue)) {
          changeCount++;
          oldValue = {};
          oldLength = 0;         
        }
        newLength = 0;
        _.forOwn(newValue, function(newVal, key) {
          newLength++;
          if (oldValue.hasOwnProperty(key)) {
            var bothNan = _.isNaN(newVal) && _.isNaN(oldValue[key]);
            if (!bothNan && oldValue[key] !== newVal) {
              changeCount++;
            }
          } else {
            changeCount++;
            oldLength++;
            oldValue[key] = newVal;
          }
        });

        if (oldLength > newLength) {
          changeCount++;
          _.forOwn(oldValue, function(oldVal, key) {
            if (!newValue.hasOwnProperty(key)) {
              oldLength--;
              delete oldValue[key];
            }
          });
        }
        oldValue = _.cloneDeep(newValue);
      }
    } else {
      if (!self.$$areEqual(newValue, oldValue, false)) {
        changeCount++;
      }
      oldValue = newValue;
    }    
    return changeCount;
  };

  var internalListenerFn = function() {
    if (firstRun) {
      listenerFn(newValue, newValue, self);
      firstRun = false;
    } else {
      listenerFn(newValue, veryOldValue, self);
    }
    
    if (trackVeryOldValue) {
      veryOldValue = _.clone(newValue);
    }
  };
  
  return this.$watch(internalWatchFn, internalListenerFn, false);
};

Scope.prototype.$on = function(name, listenerFn) {
  if (this.$$listeners[name]) {
    this.$$listeners[name].push(listenerFn);
  } else {
    this.$$listeners[name] = [listenerFn];
  }
  var listeners = this.$$listeners[name];
  return function() {
    var index = listeners.indexOf(listenerFn);
    if (index >= 0) {
      listeners[index] = null;
    }
  };
};

Scope.prototype.$emit = function(eventName) {
  var propagationStopped = false;
  var event = {
    name: eventName, 
    targetScope: this,
    stopPropagation: function() {
      propagationStopped = true;
    },
    preventDefault: function() {
      event.defaultPrevented = true;
    }
  };
  var listenerArgs = [event].concat(_.tail(arguments));
  var scope = this;
  while (scope) {
    event.currentScope = scope;
    scope.$$fireEventOnScope(eventName, listenerArgs);
    event.currentScope = null;
    scope = scope.$parent;
    if (propagationStopped) {
      break;
    }
  }
  return event;
};

Scope.prototype.$broadcast = function(eventName) {
  var event = {
    name: eventName, 
    targetScope: this,
    preventDefault: function() {
      event.defaultPrevented = true;
    }
  };
  var listenerArgs = [event].concat(_.tail(arguments));
  var scope = this;
  this.$$everyScope(function(scope) {
    event.currentScope = scope;
    scope.$$fireEventOnScope(eventName, listenerArgs);
    event.currentScope = null;
    return true;
  });
  return event;
};

Scope.prototype.$$fireEventOnScope = function(eventName, listenerArgs) {
  var listeners = this.$$listeners[eventName] || [];
  var i = 0;
  while (i < listeners.length) {
    if (listeners[i] === null) {
      listeners.splice(i, 1);
      } else {
        try {
          listeners[i].apply(null, listenerArgs);
        } catch (e) {
          console.error(e);
        }
      i++;
    }
  }
  return event;
};

Scope.prototype.$destroy = function() {
  this.$broadcast('$destroy');
  if (this.$parent) {
    var siblings = this.$parent.$$children;
    var indexOfThis = siblings.indexOf(this);
    if (indexOfThis >= 0) {
      siblings.splice(indexOfThis, 1);
    }
  }
  this.$$listeners = [];
  this.$$watchers = null;
}



function isArrayLike(obj) {
  if (_.isNull(obj) || _.isUndefined(obj)) {
    return false;
  }
  var length = obj.length;
  return length === 0 || (_.isNumber(length) && length > 0 && (length - 1) in obj);
}

module.exports = Scope;