package io.fourfinanceit.util;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

@Component
public class JsonDateDeserializer extends JsonDeserializer<Date> {
    private static final DateFormat dateFormat = new SimpleDateFormat(FormatUtils.DATE_FORMAT);

    @Override
    public Date deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        try {
            return dateFormat.parse(jsonParser.getText());
        } catch (ParseException e) {
            throw new JsonParseException(jsonParser, "Could not parse date");
        }
    }
}