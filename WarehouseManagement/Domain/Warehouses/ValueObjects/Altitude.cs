using WarehouseManagement.Shared;
using System;

namespace WarehouseManagement.Warehouses.ValueObjects
{
    public class Altitude : IValueObject
    {

        private int altitude { get; }
       

        public Altitude()
        {

        }

        public Altitude(int altitude)
        {
            this.altitude = altitude;
        }



    }



}