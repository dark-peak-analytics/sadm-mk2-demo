$(document).keyup(function(event) {
    if ($("#run_anyway_alt").is(":visible") && (event.key == "Enter")) {
        $("#run_anyway_alt").click();
    }
});