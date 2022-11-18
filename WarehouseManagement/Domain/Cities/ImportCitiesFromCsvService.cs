using System.Collections.Generic;
using EletricGo.Domain.Cities.DTO;

namespace EletricGo.Domain.Cities;

public class ImportCitiesFromCsvService
{
        private string CsvFile;

        public ImportCitiesFromCsvService(string fileName)
        {

                CsvFile = fileName;

        }

        public List<CityDto> GetAllCitiesInCsvFile()
        {
                var lines = System.IO.File.ReadAllLines(CsvFile);
                var finaList = new List<CityDto>();

                foreach(var line in lines)
                {
                        string[] columns = line.Split(',');
                        finaList.Add(new CityDto(){id = columns[1], name = columns[0]});

                }

                return finaList;
        }

}

