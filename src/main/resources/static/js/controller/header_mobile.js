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
		$(".mobilenav__overlay").css('display', 'block');
		$("#content").css('background-color','rgba(0,0,0,.2)');
		
		if($(".hQrVZb_subTab a").hasClass('active')) {
			$("#mobile-navigation").hide();
			$("#aboutSubNav").show();
		}
	});
	
	$("#closeMenuBtn").click(function (e) {
		$(".mobileNav").css('display', 'none');
		$(".mobilenav__overlay").css('display', 'none');
		$("#content").css('background-color','#fff');
	});
	
	

});