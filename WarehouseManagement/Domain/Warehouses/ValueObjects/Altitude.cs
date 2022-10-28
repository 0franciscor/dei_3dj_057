using System;
using System.Collections.Generic;
using WarehouseManagement.Domain.Shared;

namespace EletricGo.Domain.Warehouses.ValueObjects
{
    public class Altitude : IValueObject
    {

        public int altitude { get; }
       

        public Altitude()
        {

        }

        public Altitude(int altitude)
        {
            this.altitude = altitude;
        }

        public int AsInt()
        {
            return altitude;
        }
        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return altitude;
        }
    }



}