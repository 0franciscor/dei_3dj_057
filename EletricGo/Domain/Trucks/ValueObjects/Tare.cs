using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks.ValueObjects
{
    public class Tare : ValueObject
    {
        private float tareValue;

        public Tare(float tareValue)
        {
            this.tareValue = tareValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return this.tareValue;
        }

        public float asFloat()
        {
            return this.tareValue;
        }

    }    
}