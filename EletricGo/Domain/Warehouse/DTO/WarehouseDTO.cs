namespace EletricGo.Domain.Warehouse
{
    public class WarehouseDTO
    {
        public Guid warehouseID { get; set; }
        public string address { get; set; }
        public string designation { get; set; }


        public WarehouseDTO(Guid warehouseID, string adress, string designation)
        {
            this.warehouseID = warehouseID;
            this.address = address;
            this.designation = designation;
            
        }
        

    }

}