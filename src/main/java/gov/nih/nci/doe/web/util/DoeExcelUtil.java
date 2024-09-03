package gov.nih.nci.doe.web.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.poi.hssf.usermodel.HSSFDateUtil;
import org.apache.poi.ss.format.CellDateFormatter;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.parboiled.common.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import gov.nih.nci.doe.web.DoeWebException;
import gov.nih.nci.hpc.domain.metadata.HpcBulkMetadataEntries;
import gov.nih.nci.hpc.domain.metadata.HpcBulkMetadataEntry;
import gov.nih.nci.hpc.domain.metadata.HpcMetadataEntry;

public class DoeExcelUtil {

	public static HashMap<HpcBulkMetadataEntries, Map<String, String>> parseBulkMatadataEntries(
			MultipartFile metadataFile, String accessGrps, String collectionPath, String defaultGrp)
			throws IOException, DoeWebException {
		HashMap<HpcBulkMetadataEntries, Map<String, String>> entries = null;
		if (metadataFile == null || metadataFile.getName().isEmpty() || metadataFile.getOriginalFilename().isEmpty())
			return null;

		Sheet metadataSheet = getWorkbookSheet(metadataFile);
		Map<String, String> tokens = getTokensMap(metadataSheet);

		Map<String, Map<String, String>> metadataMap = getMetadataMap(metadataSheet);
		entries = buildHpcBulkMetadataEntries(metadataMap, tokens, accessGrps, collectionPath, defaultGrp);

		return entries;
	}

	private static HashMap<HpcBulkMetadataEntries, Map<String, String>> buildHpcBulkMetadataEntries(
			Map<String, Map<String, String>> metadataMap, Map<String, String> tokens, String accessGrps,
			String collectionPath, String defaultGrp) {

		HashMap<HpcBulkMetadataEntries, Map<String, String>> list = new HashMap<HpcBulkMetadataEntries, Map<String, String>>();
		HpcBulkMetadataEntries entries = new HpcBulkMetadataEntries();
		Map<String, String> assetIdentiferMapping = new HashMap<String, String>();
		List<HpcBulkMetadataEntry> pathMetadataEntries = new ArrayList<HpcBulkMetadataEntry>();
		if (metadataMap == null || metadataMap.isEmpty())
			return null;

		Iterator<String> iterator = metadataMap.keySet().iterator();
		while (iterator.hasNext()) {
			HpcBulkMetadataEntry metadataEntry = new HpcBulkMetadataEntry();
			String path = iterator.next();
			Map<String, String> metadata = metadataMap.get(path);
			path = replaceTokens(path, tokens);
			if (metadata.containsKey("collection_type") && metadata.get("collection_type").equalsIgnoreCase("Asset")) {

				// overriding the user provided access group with the parent access group since
				// it should be restricted to
				// parent access group, else the user provided access group will be taken
				// if user did not provide one, the default access group will be added
				// if no default group is assigned and the access group metadata is empty, the
				// task will fail

				String userAccessGrp = metadata.containsKey("access_group")
						&& !metadata.get("access_group").equalsIgnoreCase("public") ? metadata.get("access_group")
								: null;
				if (StringUtils.isNotEmpty(accessGrps)) {
					metadata.put("access_group", accessGrps);
				} else if (StringUtils.isEmpty(userAccessGrp) && StringUtils.isNotEmpty(defaultGrp)) {
					metadata.put("access_group", defaultGrp);

				}

				if (metadata.containsKey("asset_identifier")
						&& StringUtils.isNotEmpty(metadata.get("asset_identifier"))) {
					String assetIdentifier = metadata.get("asset_identifier");

					// set path from asset_identifier metadata
					metadataEntry.setPath(collectionPath + "/" + assetIdentifier);
					assetIdentiferMapping.put(path, assetIdentifier);
				}

			} else {
				// set file level path from asset_identifier metadata + fileName
				String parentPath = path.substring(0, path.lastIndexOf('/'));
				Map<String, String> parentMetadata = metadataMap.get(parentPath);
				String assetIdentifier = parentMetadata.get("asset_identifier");
				String fileName = path.substring(path.lastIndexOf("/"), path.length());
				metadataEntry.setPath(collectionPath + "/" + assetIdentifier + fileName);
			}

			if (metadata != null && !metadata.isEmpty()) {
				metadataEntry.getPathMetadataEntries().addAll(buildMetadataEntries(metadata));
			}
			pathMetadataEntries.add(metadataEntry);
		}
		entries.getPathsMetadataEntries().addAll(pathMetadataEntries);
		list.put(entries, assetIdentiferMapping);
		return list;
	}

	private static List<HpcMetadataEntry> buildMetadataEntries(Map<String, String> metadata) {
		List<HpcMetadataEntry> entries = new ArrayList<HpcMetadataEntry>();
		Iterator<String> iterator = metadata.keySet().iterator();
		while (iterator.hasNext()) {
			String key = iterator.next();
			String value = metadata.get(key);
			HpcMetadataEntry entry = new HpcMetadataEntry();
			entry.setAttribute(key);
			entry.setValue(value);
			entries.add(entry);
		}
		return entries;
	}

	@SuppressWarnings("resource")
	private static Sheet getWorkbookSheet(MultipartFile metadataFile) throws IOException {
		Workbook workbook = new XSSFWorkbook(metadataFile.getInputStream());
		Sheet dataSheet = workbook.getSheetAt(0);
		return dataSheet;
	}

	private static List<String> getHeader(Sheet metadataSheet) throws DoeWebException {
		List<String> header = new ArrayList<String>();
		Row firstRow = metadataSheet.getRow(metadataSheet.getFirstRowNum());

		Iterator<Cell> cellIterator = firstRow.iterator();
		while (cellIterator.hasNext()) {
			Cell currentCell = cellIterator.next();
			String cellValue = currentCell.getStringCellValue();
			if (cellValue == null || cellValue.isEmpty())
				throw new DoeWebException("Empty header column value in column " + currentCell.getColumnIndex());
			header.add(cellValue);
		}
		if (!header.contains("path") && !header.contains("Path"))
			throw new DoeWebException("Path header column is missing");
		return header;
	}

	private static Map<String, Map<String, String>> getMetadataMap(Sheet metadataSheet) throws DoeWebException {
		Map<String, Map<String, String>> metdataSheetMap = new HashMap<String, Map<String, String>>();
		Iterator<Row> iterator = metadataSheet.iterator();

		// Read 1st row which is header row with attribute names
		List<String> attrNames = getHeader(metadataSheet);
		// Read all rows (skip 1st) and construct metadata map
		// Skip cells exceeding header size
		while (iterator.hasNext()) {
			String path = null;
			Row currentRow = iterator.next();
			if (currentRow.getRowNum() == 0)
				continue;
			// Skip header row
			int counter = 0;
			Map<String, String> rowMetadata = new HashMap<String, String>();

			for (String attrName : attrNames) {
				Cell currentCell = currentRow.getCell(counter);
				counter++;
				if (currentCell == null)
					continue;
				if (attrName.equalsIgnoreCase("path")) {
					path = currentCell.getStringCellValue();
					continue;
				}
				if (currentCell.getCellType().equals(CellType.NUMERIC)) {
					double dv = currentCell.getNumericCellValue();
					if (HSSFDateUtil.isCellDateFormatted(currentCell)) {
						Date date = HSSFDateUtil.getJavaDate(dv);
						String df = currentCell.getCellStyle().getDataFormatString();
						String strValue = new CellDateFormatter(df).format(date);
						rowMetadata.put(attrName, strValue);
					} else {
						rowMetadata.put(attrName, (new Double(dv).toString()));
					}

				} else {
					if (currentCell.getStringCellValue() != null && !currentCell.getStringCellValue().isEmpty())
						rowMetadata.put(attrName, currentCell.getStringCellValue());
				}

			}

			metdataSheetMap.put(path, rowMetadata);
		}

		return metdataSheetMap;
	}

	/**
	 * Read token and value as name, value pair. Ignore empty token or value rows
	 * 
	 * @param tokenSheet
	 * @return Tokens name value pair
	 */
	private static Map<String, String> getTokensMap(Sheet tokenSheet) {
		Map<String, String> tokens = new HashMap<String, String>();
		if (tokenSheet == null)
			return tokens;

		Iterator<Row> iterator = tokenSheet.iterator();

		while (iterator.hasNext()) {

			Row currentRow = iterator.next();
			Iterator<Cell> cellIterator = currentRow.iterator();
			int count = 0;
			String token = null;
			String tokenValue = null;
			while (cellIterator.hasNext()) {

				Cell currentCell = cellIterator.next();
				String cellValue = currentCell.getStringCellValue();
				if (count == 0)
					token = cellValue;
				else
					tokenValue = cellValue;
				count++;
				// Read two column values. Tokens should have name, value pair. Nothing more
				// than that
				if (count == 2)
					break;

			}
			if (token != null && !token.isEmpty() && tokenValue != null && !tokenValue.isEmpty())
				tokens.put(token, tokenValue);
		}
		return tokens;
	}

	public static String replaceTokens(String text, Map<String, String> replacements) {
		Pattern pattern = Pattern.compile("\\{(.+?)\\}");
		Matcher matcher = pattern.matcher(text);
		StringBuffer buffer = new StringBuffer();

		while (matcher.find()) {
			String replacement = replacements.get(matcher.group(1));
			if (replacement != null) {
				matcher.appendReplacement(buffer, "");
				buffer.append(replacement);
			}
		}
		matcher.appendTail(buffer);
		return buffer.toString();
	}
}
