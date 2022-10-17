using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class Destination : ValueObject
    {
        private string Destination { get; }
        

        public Destination(string destination)
        {
            Destination = destination;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return Destination;
        }
        
    }    
}