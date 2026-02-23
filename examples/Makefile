all:
	@make property-filtering

property-filtering:
	@cabal run -- kitab -f cilium --context=k8s --filter "env==prod" -i property-filtering/in.kdl -o property-filtering

.PHONY: all $(MAKECMDGOALS)
