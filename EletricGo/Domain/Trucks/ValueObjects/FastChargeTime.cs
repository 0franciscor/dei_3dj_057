using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks.ValueObjects
{
    public class FastChargeTime : ValueObject
    {
        private float fastChargeTimeValue;

        public FastChargeTime(float fastChargeTimeValue)
        {
            this.fastChargeTimeValue = fastChargeTimeValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return this.fastChargeTimeValue;
        }

        public float asFloat()
        {
            return this.fastChargeTimeValue;
        }
    }
}