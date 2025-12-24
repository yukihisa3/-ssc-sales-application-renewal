/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                    　　     *  ./
/. *   JOB-ID      :    PSY44710                             *  ./
/. *   JOB-NAME    :    流通ＢＭＳ納品明細検収表データ作成　 *  ./
/. *               :      バロー（手書分）                   *  ./
/. *               :    13/03/22 バロー植物、追加            *  ./
/. *               :    13/04/09 相手商品名追加　            *  ./
/. ***********************************************************  ./
    PGM   (P1-?TRICD)
/.###ﾊﾟﾗﾒﾀ定義####./
/.  PARA ?KBN     ,STRING*1,IN,VALUE-' ' ./ /.1=自,2=選択,3.倉 ./
    PARA ?TRICD   ,STRING*8,IN,VALUE-'00000000'  /.取引先コード ./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*8,VALUE-'BMSTEGF'     /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMT   ,STRING*8,VALUE-'BMSTE2F'     /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'BMSFLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?JHTDENF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JHTTEGF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?JHTDENFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?JHTTEGFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./

    VAR ?PGNAME   ,NAME
    VAR ?LIBNAME  ,NAME
    VAR ?OVRFNAME ,NAME
    VAR ?PGC      ,NAME!MOD
    VAR ?PGN      ,STRING*17
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY44710'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?SOKO     ,STRING*2,VALUE-'  '          /.出荷場所./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./
    VAR ?JYUN     ,STRING*1,VALUE-' '           /.出力順./
    VAR ?DENPTN   ,STRING*1,VALUE-' '           /.伝票種類./
    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日付./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時間./
    VAR ?TORICD   ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR ?NOUDT1   ,STRING*8,VALUE-'00000000'    /.開始納品日./
    VAR ?NOUDT2   ,STRING*8,VALUE-'00000000'    /.終了納品日./
    VAR ?TENCD1   ,STRING*5,VALUE-'00000'       /.開始店舗　./
    VAR ?TENCD2   ,STRING*5,VALUE-'00000'       /.終了店舗　./
    VAR ?PGCD     ,STRING*8,VALUE-'        '    /.ﾌﾟﾛｸﾞﾗﾑCD./
    VAR ?LIBCD    ,STRING*8,VALUE-'        '    /.ﾗｲﾌﾞﾗﾘCD./
    VAR ?KEYA     ,STRING*1,VALUE-'1'           /.KEY1./
    VAR ?KEYB     ,STRING*1,VALUE-'2'           /.KEY2./
    VAR ?KEYC     ,STRING*1,VALUE-'3'           /.KEY3./
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.ﾒｯｾｰｼﾞ2    ./
    VAR ?OPR3     ,STRING*50                    /.ﾒｯｾｰｼﾞ3    ./
    VAR ?OPR4     ,STRING*50                    /.ﾒｯｾｰｼﾞ4    ./
    VAR ?OPR5     ,STRING*50                    /.ﾒｯｾｰｼﾞ5    ./

    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '流通ＢＭＳ　伝票発行'

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   BMSFLIB/TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##自倉庫／選択倉庫判定##./
/.
CHK:
    IF    ?TRICD   =    '00001302'   THEN
            ?FILNMBD := 'BMSSDN'
    END    ./

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '倉庫ｺｰﾄﾞ取得'
              GOTO ABEND
    END
  /.?MSGX :=  '倉庫＝'  && ?SOKCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '代表倉庫＝'  && ?DSOKCD
    SNDMSG    ?MSGX,TO-XCTL     ./

/.##倉庫ｺｰﾄﾞ選択##./
SKY1702I:

    ?STEP :=   'SKY1702I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRDSPF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SKY1702I.TOKELIB,PARA-(?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0    THEN
              ?PGMEC := @PGMEC
              IF    ?PGMEC  =  4010
                 THEN
                    ?MSGX := '##取消終了##'
                    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                    RETURN PGMEC-@PGMEC
                 ELSE
                    GOTO   ABEND
                 END
    END

/.##ﾌｧｲﾙ名称取得##./
/.-----------------------
FILNMCH1:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?SOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                          ---------------------------------./
    ?MSGX     :=     ?TRICD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    IF    ?TRICD   =    '00013020'   THEN
        ?FILNM    :=    ?FILNMH
    ELSE
        ?FILNM    :=    ?FILNMT
    END
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?JHTTEGF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?JHTTEGFN :=    %STRING(?JHTTEGF)
    ?MSGX     :=     ?JHTTEGFN
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##出力ファイル確認##./
PASSIGN1:

    ?STEP :=   'PASSIGN1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN FILE-?JHTTEGF!@XCL
 /. ASSIGN FILE-BMSTEGF.BMSFLIB!@XCL  ./
    IF     @PGMEC  ^=    0    THEN
           GOTO  ASSERR
    END

PASSIGN2:

    ?STEP :=   'PASSIGN2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN FILE-BRNOMTEF.BMSFLIB!@XCL
    IF     @PGMEC  ^=    0    THEN
           GOTO  ASSERR
    END

/.##バロー納品明細データ作成処理（手書）##./
SORT01:

    ?STEP :=  'SORT01'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    /. データＳＯＲＴ ./
    SORTD  INFILE-?JHTTEGF,
           OUTFILE-?JHTTEGF,
 /. SORTD  INFILE-BMSTEGF.BMSFLIB,
           OUTFILE-BMSTEGF.BMSFLIB, ./
           INRL-1020,
           KEY-1!8!DA,    /.F01  取引先CD ./
           KEY1-69!1!CA,  /.F131 商区(2:1) : 出荷先CD ./
           KEY2-54!5!PA,  /.F112 納品日./
           KEY3-70!1!CA,  /.F132 伝票(1:1) : 便 ./
           KEY4-33!5!DA,  /.F07  店舗CD ./
           KEY5-64!4!CA,  /.F12  分類CD ./
           KEY6-9!9!DA,   /.F02  自社伝票番号 ./
           KEY7-18!2!DA,  /.F03  行番号 ./
    /.     SELECT-'1!8!D!EQ!L-00013020',   ./
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END

    /. 処理前データクリア ./
    CLRFILE   FILE-BRNOMTEF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

    /. 納品明細データ作成 ./
    OVRF      FILE-JHTTEGF,TOFILE-?JHTTEGF
 /. OVRF      FILE-JHTTEGF,TOFILE-BMSTEGF.BMSFLIB    ./
    OVRF      FILE-BRNOMTE1,TOFILE-BRNOMTE1.BMSFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SYOMEIL1,TOFILE-SYOMEIL1.TOKKLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSY4471L.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '納品明細データ作成処理（手書）'
              GOTO RTN
    END

    /. 納品明細データ複写 ./
    CNVFILE   FILE-BRNOMTEF.BMSFLIB,
              TOFILE-BRNOMTSF.BMSFLIB,
              ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '納品明細データ複写処理（手書）'
              GOTO ABEND
    END

    /. 納品明細データＰＣ転送 ./
    FIMPORT   FILE-BRNOMTSF.BMSFLIB,
              TYPE-@FILE,
              PARA-BRNOMEIF,
              UNIT-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '納品明細データＰＣ転送処理（手書）'
              GOTO ABEND
    ELSE
              GOTO RTN
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ASSERR:
    ?OPR1  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '＃　他の倉庫で納品明細発行中です。　　　　　　＃'
    ?OPR3  :=  '＃　しばらくお待ち頂いてから実行して下さい。　＃'
    ?OPR4  :=  '＃　　　　　　　　　　　　　　　　　　　　　　＃'
    ?OPR5  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL     OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    GOTO PASSIGN1

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
