package gov.nih.nci.doe.web.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;

public class JsonDiff {
	public static JsonNode getJsonDiff(String json1, String json2) throws IOException {
		ObjectMapper mapper = new ObjectMapper();
		JsonNode node1 = mapper.readTree(json1);
		JsonNode node2 = mapper.readTree(json2);
		return getJsonDiff(node1, node2);
	}

	private static JsonNode getJsonDiff(JsonNode node1, JsonNode node2) {
		if (node1.equals(node2)) {
			return null;
		}
		if (node1.isObject() && node2.isObject()) {
			ObjectNode diff = JsonNodeFactory.instance.objectNode();
			node1.fields().forEachRemaining(entry -> {
				String key = entry.getKey();
				JsonNode value1 = entry.getValue();
				JsonNode value2 = node2.get(key);
				JsonNode diffNode = getJsonDiff(value1, value2);
				if (diffNode != null) {
					diff.set(key, diffNode);
				}
			});
			node2.fields().forEachRemaining(entry -> {
				String key = entry.getKey();
				if (!node1.has(key)) {
					diff.set(key, entry.getValue());
				}
			});
			return diff.size() > 0 ? diff : null;
		}
		if (node1.isArray() && node2.isArray()) {
			ArrayNode diff = JsonNodeFactory.instance.arrayNode();
			for (int i = 0; i < node1.size() || i < node2.size(); i++) {
				JsonNode diffNode = getJsonDiff(node1.get(i), node2.get(i));
				if (diffNode != null) {
					diff.add(diffNode);
				}
			}
			return diff.size() > 0 ? diff : null;
		}
		return node2;
	}
}
