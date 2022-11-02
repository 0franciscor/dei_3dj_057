using EletricGo.Controllers;
using EletricGo.Domain.Deliveries;
using Moq;
using EletricGo.Domain.Shared;

namespace DDDNetCoreTests.Controllers.Deliveries
{
    internal class DeliveryControllerTest
    {
        private string? id;

        private DateTime deliveryDate;

        private float loadTime;

        private float unloadTime;

        private string? destination;

        private float deliveryMass;

        [SetUp]
        public void Setup()
        {
            id = "testID";
            deliveryDate = new DateTime(2020, 12, 12);
            loadTime = 10;
            unloadTime = 20;
            destination = "testDestination";
            deliveryMass = 30;

        }

        

    }
}
