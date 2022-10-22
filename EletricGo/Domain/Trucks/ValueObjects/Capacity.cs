using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks.ValueObjects
{
    public class Capacity : ValueObject
    {
        private float capacityValue;

        public Capacity(float capacityValue)
        {
            this.capacityValue = capacityValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return this.capacityValue;
        }

        public float asFloat()
        {
            return this.capacityValue;
        }

    }   
} 
