using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class Tare : ValueObject
    {
        private float TareValue { get; }

        public Tare(float tareValue)
        {
            TareValue = tareValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return TareValue;
        }
    }    
}