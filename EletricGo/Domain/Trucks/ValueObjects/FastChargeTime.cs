using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class FastChargeTime : ValueObject
    {
        private float FastChargeTimeValue { get; }

        public FastChargeTime(float fastChargeTimeValue)
        {
            FastChargeTimeValue = fastChargeTimeValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return FastChargeTimeValue;
        }
    }
}