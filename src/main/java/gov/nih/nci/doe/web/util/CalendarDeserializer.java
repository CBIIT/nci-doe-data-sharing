package gov.nih.nci.doe.web.util;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

public class CalendarDeserializer extends JsonDeserializer<Calendar> {

    private static final String DATE_FORMAT = "yyyy-MM-dd-HH:mm";
    private static final String DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss-HH:mm";

    @Override
    public Calendar deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        Calendar calendar = Calendar.getInstance();

        // Handle date string
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC")); // Set appropriate timezone
        try {
            calendar.setTime(sdf.parse(p.getText()));
        } catch (ParseException e) {
            try {
                sdf = new SimpleDateFormat(DATE_TIME_FORMAT);
                calendar.setTime(sdf.parse(p.getText()));
            } catch (ParseException ex) {
                throw new IOException("Failed to parse date string: " + p.getText(), ex);
            }
        }

        return calendar;
    }
}