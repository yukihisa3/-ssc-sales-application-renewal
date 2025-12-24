/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    CGEDATBK                             *  ./
/. *   JOB-NAME    :    退避処理（伝票データ）               *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CGEDATBK'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM     ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  月次更新前処理起動確認                                      ./
STEP00:

    ?OPR1  :=  '　＃＃＃＃＃月　次　更　新　前　処　理＃＃＃＃＃　'
    ?OPR2  :=  '　月次更新前処理を開始致します。'
    ?OPR3  :=  '　　　　　　　　　　　　　　'
    ?OPR4  :=  '［確認］本社倉庫は全て終了済ですか？確認ＯＫ？　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.  SNDMSG MSG-'＃売上Ｆ　圧縮　　開始＃',TO-XCTL
    DACTLF    SHTDENL1.TOKFLIB
    DACTLF    SHTDENL2.TOKFLIB
    DACTLF    SHTDENL3.TOKFLIB
    DACTLF    SHTDENL4.TOKFLIB
    DACTLF    SHTDENL5.TOKFLIB
    DACTLF    SHTDENL6.TOKFLIB
    DACTLF    SHTDENL9.TOKFLIB
    DACTLF    SHTDENLA.TOKFLIB
    DACTLF    SHTDENLB.TOKFLIB
    DACTLF    SHTDENLC.TOKFLIB
    DACTLF    SHTDENLD.TOKFLIB
    CNDPF     FILE-SHTDENF.TOKFLIB
    ACTLF     SHTDENL1.TOKFLIB
    ACTLF     SHTDENL2.TOKFLIB
    ACTLF     SHTDENL3.TOKFLIB
    ACTLF     SHTDENL4.TOKFLIB
    ACTLF     SHTDENL5.TOKFLIB
    ACTLF     SHTDENL6.TOKFLIB
    ACTLF     SHTDENL9.TOKFLIB
    ACTLF     SHTDENLA.TOKFLIB
    ACTLF     SHTDENLB.TOKFLIB
    ACTLF     SHTDENLC.TOKFLIB
    ACTLF     SHTDENLD.TOKFLIB
    SNDMSG MSG-'＃売上Ｆ　圧縮　　終了＃',TO-XCTL
    SNDMSG MSG-'＃入出庫Ｆ　圧縮　開始＃',TO-XCTL
    DACTLF    FILE-NYSFILL1.TOKFLIB
    DACTLF    FILE-NYSFILL2.TOKFLIB
    CNDPF     FILE-NYSFILF.TOKFLIB
    ACTLF     FILE-NYSFILL1.TOKFLIB
    ACTLF     FILE-NYSFILL2.TOKFLIB
    SNDMSG MSG-'＃入出庫Ｆ　圧縮　終了＃',TO-XCTL
  ./
/.   退避処理（伝票データ）                                     ./
TAIHI1:

    ?STEP :=   'TAIHI1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  SNDMSG MSG-'＃売上Ｆ　　　　　退避中＃',TO-XCTL
    SAVFILE FILE-SHTDENF.TOKFLIB,TODEV-MO,ADD-@NO,MODE-@USED
    SNDMSG MSG-'＃在庫マスタ　　　退避中＃',TO-XCTL
    SAVFILE FILE-ZAMZAIF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃在庫マスタ実績　退避中＃',TO-XCTL
    SAVFILE FILE-ZAMJISF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃入庫Ｆ　　　　　退避中＃',TO-XCTL
    SAVFILE FILE-NYKFILF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃入出庫Ｆ　　　　退避中＃',TO-XCTL
    SAVFILE FILE-NYSFILF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃作業実績Ｆ　　　退避中＃',TO-XCTL
    SAVFILE FILE-SGYFILF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃請求合計Ｆ　　　退避中＃',TO-XCTL
    SAVFILE FILE-SETGKFA.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃条件Ｆ　　　　　退避中＃',TO-XCTL
    SAVFILE FILE-HJYOKEN.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃発注ヘッダＦ　　退避中＃',TO-XCTL
    SAVFILE FILE-HACHEDF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃発注明細Ｆ　　　退避中＃',TO-XCTL
    SAVFILE FILE-HACMEIF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃取引先Ｍ　　　　退避中＃',TO-XCTL
    SAVFILE FILE-HTOKMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃店舗Ｍ　　　　　退避中＃',TO-XCTL
    SAVFILE FILE-HTENMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃商品名称Ｍ　　　退避中＃',TO-XCTL
    SAVFILE FILE-HMEIMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃商品変換ＴＢＬ　退避中＃',TO-XCTL
    SAVFILE FILE-HSHOTBL.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃倉庫Ｍ　　　　　退避中＃',TO-XCTL
    SAVFILE FILE-HSOKMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃作業区分Ｍ　　　退避中＃',TO-XCTL
    SAVFILE FILE-SGYKBMF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃仕入先Ｍ　　　　退避中＃',TO-XCTL
    SAVFILE FILE-ZSHIMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
 ./
    SNDMSG MSG-'＃＃＃全データＤＡＴ退避開始＃＃＃',TO-XCTL
    SNDMSG MSG-'＃ＴＯＫＦＬＩＢ　退避中＃',TO-XCTL
 /. SAVLIB LIB-TOKFLIB,TODEV-DAT,ADD-@NO,REWIND-@NO,MODE-@USED./
    SAVLIB LIB-TOKFLIB/TOKKLIB,TODEV-LTO,MODE-@USED
    SNDMSG MSG-'＃＃＃全データＤＡＴ退避終了＃＃＃',TO-XCTL
 /.
    SNDMSG MSG-'＃受払累積データ　退避中＃',TO-XCTL
    SAVFILE FILE-RUISEKF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃検品環境データ　退避中＃',TO-XCTL
    SAVFILE FILE-KENKANF.TOKKLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃検品検収データ　退避中＃',TO-XCTL
    SAVFILE FILE-KENSOKF.TOKKLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃検品梱包データ　退避中＃',TO-XCTL
    SAVFILE FILE-RUIKONF.TOKKLIB,TODEV-MO,ADD-@YES,MODE-@USED
    SNDMSG MSG-'＃検品情報データ　退避中＃',TO-XCTL
    SAVFILE FILE-RUISYUF.TOKKLIB,TODEV-MO,ADD-@YES,MODE-@USED
   ./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃＃　月次更新前処理　終了　＃＃＃',TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
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