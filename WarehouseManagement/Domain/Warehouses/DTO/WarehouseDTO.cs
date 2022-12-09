
namespace EletricGo.Domain.Warehouses.DTO
{
	public class WarehouseDto
	{

		public string Id { get; set; }
		public string Address { get; set; }
		public int Altitude { get; set; }
		public string Latitude { get; set; }
		public string Longitude { get; set; }
		public string Designation { get; set; }
		
		public string City { get; set; }

		public bool Active { get; set; }
		

	}
}