using System.Text.RegularExpressions;
using EletricGo.Domain.Cities;
using EletricGo.Domain.Cities.ValueObjects;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;

namespace EletricGo.Domain.Warehouses 
{
    public class Warehouse : Entity<WarehouseId>, IAggregateRoot
    {
        
        public Address Address { get; set; }
        public Altitude Altitude { get; set; }
        public Coordinates Coordinates{ get; set; }
        public Designation Designation { get; set; }

        public CityId cityId { get; set;}

        public Warehouse() { }
        
        public Warehouse(EntityID id, Address address, Altitude altitude, Coordinates coordinates, Designation designation)
        {
            this.Id = id.Value;
            this.Address = address;
            this.Altitude = altitude;
            this.Coordinates = coordinates;
            this.Designation = designation;
            
        }
        public Warehouse(EntityID id, Address address, Altitude altitude, Coordinates coordinates, Designation designation, CityId city)
        
        {
            this.Id = id.Value;
            this.Address = address;
            this.Altitude = altitude;
            this.Coordinates = coordinates;
            this.Designation = designation;
            this.cityId = city;
        }

        public Warehouse(WarehouseDto dto)
        {
            this.Address = new Address(dto.Address);
            if (string.IsNullOrEmpty(dto.Id))
            {
                throw new BusinessRuleValidationException("The Id can't be null or empty");
            }
            if (dto.Id.Length != 3)
            {
                throw new BusinessRuleValidationException("The Id must have only three characters");
            }
            
            if (int.TryParse(dto.Id, out _))
            {
                throw new BusinessRuleValidationException("The Id must be alphanumeric");
            }

            if (Regex.IsMatch(dto.Id, @"^[a-zA-Z]+$"))
            {
                throw new BusinessRuleValidationException("The Id must be alphanumeric");
            }
            this.Id = dto.Id;
            this.Altitude = new Altitude(dto.Altitude);
            this.Coordinates = new Coordinates(dto.Latitude, dto.Longitude);
            this.Designation = new Designation(dto.Designation);

        }

        public WarehouseDto ToWarehouseDto()
        {
            return new WarehouseDto(){Id = this.Id.ToString(), Address = this.Address.fullAddress, Altitude = this.Altitude.AsInt(), Latitude = this.Coordinates.AsStringLatitude(), Longitude = this.Coordinates.AsStringLongitude(), Designation = this.Designation.AsString(), City = cityId.Id};
        }

        public void Update(WarehouseDto dto)
        {
            if (dto.Address != null)
            {
                this.Address = new Address(dto.Address);
            }

            if (dto.Altitude != default(int))
            {
                this.Altitude = new Altitude(dto.Altitude);                    
            }

            if (dto.Latitude != null && dto.Longitude != null)
            {
                this.Coordinates = new Coordinates(dto.Latitude, dto.Longitude);
            }

            if (dto.Designation != null)
            {
                this.Designation = new Designation(dto.Designation);    
            }
            
            
        }

        public void AssociateCityWithWarehouse(CityId city)
        {
            this.cityId = city;
        }

    }
}