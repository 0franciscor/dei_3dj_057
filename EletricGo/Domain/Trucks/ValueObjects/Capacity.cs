using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class Capacity : ValueObject
    {
        private float CapacityValue { get; }

        public Capacity(float capacityValue)
        {
            CapacityValue = capacityValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return CapacityValue;
        }

    }   
} 
