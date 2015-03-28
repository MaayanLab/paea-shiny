// Change status of the action button depending on the message

Shiny.addCustomMessageHandler("button_status_message",
    function(message) {
        if(message.disable) {
            $(message.id).prop( 'disabled', 'true');
        } else {
            $(message.id).removeProp('disabled');
        }
    }
);   