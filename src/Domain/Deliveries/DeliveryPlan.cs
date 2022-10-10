public class DeliveryPlan : Entity<Delivery>, Entity<Truck>
{

    private DeliveryPlan()
    {
    
    }

    public DeliveryPlan(string deliveryID, string truckID)
    {
        this.delivery=delivery;
        this.truck=truck;
    }

    public void ChangeDescription(string description)
    {
        if (!this.Active)
            throw new BusinessRuleValidationException("It is not possible to change the description to an inactive family.");
        this.Description = description;
    }
    public void MarkAsInative()
    {
        this.Active = false;
    }
}
