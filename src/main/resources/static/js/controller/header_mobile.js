$(document).ready(function () {

	$("#aboutMenuMobile").click(function (e){
		
		$("#mobile-navigation").hide();
		$("#aboutSubNav").show();
	});
	
	$("#backToMainMenu").click(function (e) {
		$("#mobile-navigation").show();
		$("#aboutSubNav").hide();
	});
	
	$("#menuBtn").click(function (e) {
		$(".mobileNav").css('display', 'flex');
		
		if($(".hQrVZb_subTab a").hasClass('active')) {
			$("#mobile-navigation").hide();
			$("#aboutSubNav").show();
		}
	});
	
	$("#closeMenuBtn").click(function (e) {
		$(".mobileNav").css('display', 'none');
	});
	
	

});