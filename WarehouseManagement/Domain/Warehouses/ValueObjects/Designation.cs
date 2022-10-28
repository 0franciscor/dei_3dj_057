using System;
using System.Collections.Generic;
using WarehouseManagement.Domain.Shared;

namespace EletricGo.Domain.Warehouses.ValueObjects
{
    public class Designation : IValueObject
    {

        public string designation { get; }

        public Designation()
        {

        }

        public Designation(string designation)
        {
            this.designation = designation;
        }

        public string AsString()
        {
            return designation;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return designation;
        }
    }



}