using System;
using System.Collections.Generic;
using WarehouseManagement.Domain.Shared;

namespace EletricGo.Domain.Warehouses.ValueObjects
{
    public class Address : IValueObject
    {
        
        public string address { get; }
        public int number { get; }
        public string zipCode { get; }
        public string location { get; }
        
        public string fullAddress { get; }

        public Address()
        {
            
        }
        
        public Address(string address, int number, string zipCode, string location)
        {
            this.address = address;
            this.number = number;
            this.zipCode = zipCode;
            this.location = location;
            this.fullAddress = address + "," + number + "," + zipCode + "," + location;
        }

        public Address(string address)
        {
            this.fullAddress = address;
            var aux = address.Split(",");
            this.address = aux[0];
            this.number = int.Parse(aux[1]);
            this.zipCode = aux[2];
            this.location = aux[3];
            
        }

        public string AsString()
        {
            return address + "," + number + "," + location + "," + zipCode;
        }


        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return address;
            yield return number;
            yield return zipCode;
            yield return location;
        }
    }
        
    
    
}