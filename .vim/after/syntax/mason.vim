" Booking.com's Mason code has JavaScript inside <%method js_inline>
unlet b:current_syntax
syn include @javascript syntax/javascript.vim
syn region masonJS matchgroup=Delimiter start="<%method js_inline[^>]*>" end="</%method>" contains=@javascript,masonExpr
