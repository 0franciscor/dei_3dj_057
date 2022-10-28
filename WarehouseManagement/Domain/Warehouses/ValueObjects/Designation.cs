using WarehouseManagement.Shared;
using System;

namespace WarehouseManagement.Warehouses.ValueObjects
{
    public class Designation : IValueObject
    {

        private string designation { get; }

        public Designation()
        {

        }

        public Designation(string designation)
        {
            this.designation = designation;
        }



    }



}