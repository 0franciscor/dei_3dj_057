using System;
using System.Collections.Generic;
using WarehouseManagement.Domain.Shared;

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
            this.latitude = latitude;
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