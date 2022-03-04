package gov.nih.nci.doe.web.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.ModelInfo;

public interface ModelInfoRpository extends JpaRepository<ModelInfo, String> {

	@Query("select a from ModelInfo a where a.assetPath =?1")
	ModelInfo getModelInfoByModelPath(String modelPath);

	@Query("select a from ModelInfo a")
	List<ModelInfo> getAllModelInfoPaths();
}
