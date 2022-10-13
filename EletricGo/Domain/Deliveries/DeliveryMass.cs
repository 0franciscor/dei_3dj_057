using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryMass : ValueObject
    {

        private float mass;

        public DeliveryMass(float mass)
        {
            this.mass = mass;
        }

        public String toString()
        {
            return mass.ToString();
        }
        
        public Boolean equals(Object obj)
        {
            return true;
        }
        
        public int hashCode()
        {
            return 0;
        }

        public float AsFloat()
        {
            return mass;
        }

    }    
}