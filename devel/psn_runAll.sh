
## maybe delete all lst's first, or just look at modification times after?    
	   
for mod in `ls *.{mod,ctl}` ; do echo $mod ; sleep 2; (execute $mod &) ; done

## xgxr016 doesn't run because an input item is being overwritten

## then go to subdir xgxr001dir
nmfe74 input.txt output.txt

## 
