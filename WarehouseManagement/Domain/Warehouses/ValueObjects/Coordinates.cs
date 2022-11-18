using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
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

            if (string.IsNullOrEmpty(latitude) || string.IsNullOrEmpty(longitude))
            {
                throw new BusinessRuleValidationException("The latitude or longitude can't be null or empty");
            }

            
            if (!Regex.IsMatch(latitude,"-*([0-9]|[1-8][0-9]|90).[0-9]{4}º [N|S]"))
            {
                throw new BusinessRuleValidationException("The latitude must be in the following format XX.XXXXº N");
            }
            this.latitude = latitude;
            

            if (!Regex.IsMatch(longitude,"-*([0-9]|[1-9][0-9]|1[0-7][0-9]|180).[0-9]{4}º [W|E]"))
            {
                throw new BusinessRuleValidationException("The longitude must be in the following format XX.XXXXº W");
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