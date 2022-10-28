using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;

namespace EletricGo.Domain.Warehouses 
{
    public class Warehouse : Entity<WarehouseID>, IAggregateRoot
    {
        public Address Address { get; set; }
        public Altitude Altitude { get; set; }
        public Coordinates Coordinates{ get; set; }
        public Designation Designation { get; set; }  

        public Warehouse() { }
        public Warehouse(Address address, Altitude altitude, Coordinates coordinates, Designation designation)
        {
            this.Address = address;
            this.Altitude = altitude;
            this.Coordinates = coordinates;
            this.Designation = designation;
        }

        public Warehouse(WarehouseDto dto)
        {
            this.Address = new Address(dto.Address);
            this.Id = dto.Id;
            this.Altitude = new Altitude(dto.Altitude);
            this.Coordinates = new Coordinates(dto.Latitude, dto.Longitude);
            this.Designation = new Designation(dto.Designation);
        }

        public WarehouseDto ToWarehouseDto()
        {
            return new WarehouseDto(){Id = this.Id, Address = this.Address.AsString(), Altitude = this.Altitude.AsInt(), Latitude = this.Coordinates.AsStringLatitude(), Longitude = this.Coordinates.AsStringLongitude(), Designation = this.Designation.AsString()};
        }

        public void Update(WarehouseDto dto)
        {
            this.Address = new Address(dto.Address);
            this.Altitude = new Altitude(dto.Altitude);
            this.Coordinates = new Coordinates(dto.Latitude, dto.Longitude);
            this.Designation = new Designation(dto.Designation);
            
        }

    }
}