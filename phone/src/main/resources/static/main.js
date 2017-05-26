angular.module('app', []).controller('PhoneController', PhoneController);

angular.element(function () {
	angular.bootstrap(document, ['app']);
});

function PhoneController($http) {
	var ctrl = this;

	ctrl.submit = function (data) {
		// reset form
		if (ctrl.errors) ctrl.errors = undefined;
		if (ctrl.data) ctrl.data = undefined;

		$http.post('phones', data).then(
			function (res) {
				ctrl.data = res.data;
			},
			function (res) {
				ctrl.errors = res.data;
			}
		)
	}
};

PhoneController.$inject = ['$http'];