using System;
using System.Collections.Generic;
using EletricGo.Domain.Shared;

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
            if (designation.Length > 50)
            {
                throw new BusinessRuleValidationException("The maximum length for description is 50 characters");
            }
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