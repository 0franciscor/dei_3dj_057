using System;
using EletricGo.Domain.Shared;
//using Newtonsoft.Json;

namespace EletricGo.Domain.Warehouse
{
    public class WarehouseAddress : ValueObject
    {
        private string Street { get; }
        private string Number { get; }
        private string City { get; }
        private string Country { get; }
        private string ZipCode { get; }

        public WarehouseAddress(string street, string number, string city, string country, string zipCode)
        {
            Street = street;
            Number = number;
            City = city;
            Country = country;
            ZipCode = zipCode;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return Street;
            yield return Number;
            yield return City;
            yield return Country;
            yield return ZipCode;
        }
    }
}