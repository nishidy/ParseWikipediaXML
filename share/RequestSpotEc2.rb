require 'aws-sdk-core'
require 'active_support/all'

ec2 = Aws::EC2::Client.new( region: 'us-west-2' )

#=begin
resp = ec2.request_spot_instances({
	spot_price: "0.2",
	valid_until: 3.hour.since,
	launch_specification: {
		image_id: "ami-44da5574",
		key_name: "was",
		instance_type: "c4.4xlarge",
		placement: {
			availability_zone: "us-west-2a"
		}
	}
})

#p resp

break_flag = false
while true do
	ec2.describe_spot_instance_requests.spot_instance_requests.each { |req|
		if req.state == "active"
			break_flag = true
			break
		end
		#p req.status.code
		print "."
	}
	break if break_flag
	sleep 5
end
#=end

`echo [ec2-spot-instance] > playbooks/hosts` 
ec2.describe_instances.reservations.each { |res|
	res.instances.each { |ins|
		if ins.state.name == "running"
			if ins.instance_lifecycle == "spot"
				`echo "#{ins.public_dns_name}" >> playbooks/hosts` 
				puts "Added #{ins.public_dns_name} to inventory."
			end
		end
	}
}

`ansible-playbook -i playbooks/hosts -u ec2-user --private-key ~/.ssh/aws_rsa playbooks/setup.yml`

