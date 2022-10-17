using System;
using EletricGo.Domain.Shared;
//using Newtonsoft.Json;

namespace EletricGo.Domain.Warehouse
{
    public class WarehouseDesignation : ValueObject
    {
        private string Designation { get; }

        public WarehouseDesignation(string designation)
        {
            Designation = designation;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return Designation;
        }
       
    }
}