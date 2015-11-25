require 'aws-sdk-core'
require 'active_support/all'

# Show message
def decop(text)
    deco="==="
    puts "#{deco} #{text} #{deco}"
end

# Wait on showing message with motion
def waitp(text,sleept)
    cnt = 0
    max_cnt=10
    while cnt<max_cnt do
        $stdout.flush
        print "=== "+text
        print " ["
        print " "*cnt
        print "-"
        print " "*(max_cnt-cnt-1)
        print "] ===\r"
        cnt += 1
        sleep sleept*1.0/max_cnt
    end
end

ec2 = Aws::EC2::Client.new( region: 'us-west-2' )

req_ins_type = "c3.2xlarge"
resp = ec2.describe_spot_price_history({
	start_time: Time.now,
	end_time: Time.now,
	max_results:3,
	instance_types: [req_ins_type]
})

min_spot = resp.spot_price_history.inject{ |max_spot, spot|
	spot.spot_price.to_f < max_spot.spot_price.to_f ? spot : max_spot
}

req_spot_price = min_spot.spot_price.to_f+0.02
req_avail_zone = min_spot.availability_zone

decop "Request spot price #{req_spot_price} with #{req_ins_type} at #{req_avail_zone}"

resp = ec2.request_spot_instances({
    spot_price: "#{req_spot_price}",
    valid_until: 1.hour.since,
    launch_specification: {
        image_id: "ami-44da5574",
        key_name: "was",
        instance_type: req_ins_type,
        placement: {
            availability_zone: req_avail_zone
        },
        security_groups: [ "launch-wizard-1" ]
    }
})
p resp
decop "Requested the spot instance"

ins_id = ""
bflag = false
while true do
    ec2.describe_spot_instance_requests.spot_instance_requests.each { |req|
        if req.state == "active"
            ins_id = req.instance_id
            bflag = true
            break
        end
    }

    waitp("Waiting for the instance to be active",5)
    break if bflag
end

$stdout.flush
puts ""
decop "Found active spot instance"

bflag = false
`echo [ec2-spot-instance] > playbooks/hosts`
while true do
    ins = ec2.describe_instances({instance_ids:[ins_id]}).reservations[0].instances[0]
    if ins.state.name == "running" && ins.instance_lifecycle == "spot"
        st = ec2.describe_instance_status({instance_ids:[ins_id]}).instance_statuses[0]
        if st.system_status.status == "ok" && st.instance_status.status == "ok"
            decop "The status of the spot instance #{ins.public_dns_name} is ok"
            `echo "#{ins.public_dns_name}" >> playbooks/hosts`
            bflag = true
        end
    end

    waitp("Waiting for status of the instance to be ok",5)
    break if bflag
end

$stdout.flush
puts ""
decop "Build environmet by ansible"
`ansible-playbook -i playbooks/hosts -u ec2-user --private-key ~/.ssh/aws_rsa playbooks/setup.yml`

