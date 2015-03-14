function bidClick() {
    navigator.id.getVerifiedEmail(function(assertion) {
        if (assertion) {
            document.location = "/account/" + assertion;
        }
        else {}
    });
}