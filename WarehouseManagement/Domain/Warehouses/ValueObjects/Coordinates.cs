using System;
using System.Collections.Generic;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Warehouses.ValueObjects
{
    public class Coordinates : IValueObject
    {

        public string latitude { get; }
        public string longitude { get; }

        public Coordinates()
        {

        }

        public Coordinates(string latitude, string longitude)
        {
            double aux;
            
            if (!double.TryParse(latitude, out aux) || aux is > 90 or < -90)
            {
                throw new BusinessRuleValidationException("The latitude must be in the range [-90,90]");
            }
            this.latitude = latitude;
            
            if (!double.TryParse(longitude, out aux) || aux is > 180 or < -180)
            {
                throw new BusinessRuleValidationException("The longitude must be in the range [-180,180]");
            }
            this.longitude = longitude;
        }

        public string AsStringLatitude()
        {
            return latitude;
        }
        public string AsStringLongitude()
        {
            return longitude;
        }


        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return latitude;
            yield return longitude;
        }
    }



}