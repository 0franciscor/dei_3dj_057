using WarehouseManagement.Shared;
using System;

namespace WarehouseManagement.Warehouses.ValueObjects
{
    public class Address : IValueObject
    {
        
        private string address { get; }
        private int number { get; }
        private string zipCode { get; }
        private string location { get; }

        public Address()
        {
            
        }
        
        public Address(string address, int number, string zipcode, string location)
        {
            this.address = address;
            this.number = number;
            this.zipCode = zipCode;
            this.location = location
        }
        
        
        
    }
        
    
    
}