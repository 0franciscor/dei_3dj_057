using System;
using EletricGo.Domain.Shared;
//using Newtonsoft.Json;

namespace EletricGo.Domain.Warehouse
{
    public class WarehouseAddress : ValueObject
    {
        private string Street { get; }
        private int Number { get; }
        private string City { get; }
        private string Country { get; }
        private string ZipCode { get; }

        public WarehouseAddress(string address)
        {
            String[] aux = address.Split(',');
            Street = aux[0];
            Number = aux[1];
            City = aux[2];
            Country = aux[3];
            ZipCode = aux[4];
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