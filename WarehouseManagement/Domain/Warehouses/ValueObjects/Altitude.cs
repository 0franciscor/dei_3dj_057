using System;
using System.Collections.Generic;
using EletricGo.Domain.Shared;

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
            if (altitude is < 0 or > 13000)
            {
                throw new BusinessRuleValidationException("The altitude can't be a negative number");
            }
            
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