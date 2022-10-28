using WarehouseManagement.Shared;
using System;

namespace WarehouseManagement.Warehouses.ValueObjects
{
    public class Coordinates : IValueObject
    {

        private string latitude { get; }
        private string longitude { get; }

        public Coordinates()
        {

        }

        public Coordinates(string latitude, string longitude)
        {
            this.latitude = latitude;
            this.longitude = longitude;
        }



    }



}