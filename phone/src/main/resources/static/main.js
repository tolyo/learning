angular.module('app', []).controller('PhoneController', PhoneController);

angular.element(function () {
	angular.bootstrap(document, ['app']);
});

function PhoneController() {
	this.test = "hello"
};