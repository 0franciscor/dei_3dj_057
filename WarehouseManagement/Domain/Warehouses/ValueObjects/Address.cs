using System;
using System.Collections.Generic;
using System.Globalization;
using EletricGo.Domain.Shared;
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
            this.fullAddress = address ?? throw new BusinessRuleValidationException("The address can't be null");
            var aux = address.Split(",");
            this.address = aux[0];
            if (int.Parse(aux[1]) < 0)
            {
                throw new BusinessRuleValidationException("The address number can't be negative");
            }
            this.number = int.Parse(aux[1]);
            
            if (aux[2].Length != 7)
            {
                throw new BusinessRuleValidationException("The zip code isn't in the right format");
            }
            
            if (!int.TryParse(aux[2][..4], out _) || !int.TryParse(aux[2].AsSpan(4,3), out _))
            {
                throw new BusinessRuleValidationException("The zip code isn't in the right format");
            }

            if (!aux[2].Substring(3,1).Equals("-"))
            {
                throw new BusinessRuleValidationException("The zip code isn't in the right format");
            }
            this.zipCode = aux[2];

            if (aux[3].Length > 40)
            {
                throw new BusinessRuleValidationException("The location isn't in the right format");
            }
            
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