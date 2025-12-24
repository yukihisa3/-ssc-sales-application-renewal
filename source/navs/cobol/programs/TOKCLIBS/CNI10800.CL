/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    月次更新処理　　　　                 *  ./
/. *   JOB-ID      :    CNI10800                             *  ./
/. *   JOB-NAME    :    月次更新その他処理                   *  ./
/. ***********************************************************  ./
    PGM  (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*2,OUT,VALUE-'  ' /.OK,NG./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CNI10700'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
    VAR       ?OK       ,STRING*2,VALUE-'OK'        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?NG       ,STRING*2,VALUE-'NG'        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./

/.PG名称ｾｯﾄ./

    ?PGNM  :=  '月次更新（その他）'

/.ﾗｲﾌﾞﾗﾘﾘｽﾄ登録./

    DEFLIBL TOKELIB/TOKFLIB

/.前回の不要８０行目削除データバックアップ./
PCNVFILE:

    ?STEP :=   'PCNVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃前回分不要８０行データコピー＃',TO-XCTL

    CNVFILE FILE-DENDELF.NAVWWLIB,TOFILE-DENDELFB.NAVWWLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【前回分データコピー】'
              GOTO ABEND END

/.不要８０行目削除処理./
SOS2340B:

    ?STEP :=   'SOS2340B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃不要８０行削除処理＃',TO-XCTL

    OVRF FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF FILE-DENDELF,TOFILE-DENDELF.NAVWWLIB
    CALL  SOS2340B.SOSLIBB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【不要８０行削除処理】'
              GOTO ABEND END

/.##出荷検品ＤＴ削除##./
SSY8797B:

    ?STEP :=   'SSY8797B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃出荷ＤＴ　削除中＃',TO-XCTL

    OVRF FILE-DJSYUKL1,TOFILE-DJSYUKL1.TOKKLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SSY8797B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷ＤＴ　削除処理】'
              GOTO ABEND END

/.##基本情報ＤＴ削除##./
SSY8796B:

    ?STEP :=   'SSY8796B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃基本情報　削除中＃',TO-XCTL

    OVRF FILE-DJJOHOL1,TOFILE-DJJOHOL1.TOKKLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SSY8796B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　削除処理】'
              GOTO ABEND END

/.##出荷Ｆ圧縮##./
PCNDPF1:

    ?STEP :=   'PCNDPF1 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃出荷情報　圧縮中＃',TO-XCTL

    DACTLF FILE-DJSYUKL1.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　切離１　】'
              GOTO ABEND END
    DACTLF FILE-DJSYUKL2.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　切離２　】'
              GOTO ABEND END
    DACTLF FILE-DJSYUKL3.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　切離３　】'
              GOTO ABEND END
    DACTLF FILE-DJSYUKL4.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　切離４　】'
              GOTO ABEND END
    DACTLF FILE-DJSYUKL5.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　切離５　】'
              GOTO ABEND END
    DACTLF FILE-DJSYUKL7.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　切離７　】'
              GOTO ABEND END
    DACTLF FILE-DJSYUKL8.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　切離８　】'
              GOTO ABEND END
    CNDPF FILE-DJSYUKF.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　圧縮中　】'
              GOTO ABEND END
    ACTLF FILE-DJSYUKL1.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　関連１　】'
              GOTO ABEND END
    ACTLF FILE-DJSYUKL2.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　関連２　】'
              GOTO ABEND END
    ACTLF FILE-DJSYUKL3.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　関連３　】'
              GOTO ABEND END
    ACTLF FILE-DJSYUKL4.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　関連４　】'
              GOTO ABEND END
    ACTLF FILE-DJSYUKL5.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　関連５　】'
              GOTO ABEND END
    ACTLF FILE-DJSYUKL7.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　関連７　】'
              GOTO ABEND END
    ACTLF FILE-DJSYUKL8.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【出荷情報　関連８　】'
              GOTO ABEND END

/.##情報Ｆ圧縮##./
PCNDPF2:

    ?STEP :=   'PCNDPF2 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃基本情報　圧縮中＃',TO-XCTL

    DACTLF FILE-DJJOHOL1.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　切離１　】'
              GOTO ABEND END
    DACTLF FILE-DJJOHOL2.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　切離２　】'
              GOTO ABEND END
    DACTLF FILE-DJJOHOL3.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　切離３　】'
              GOTO ABEND END
    DACTLF FILE-DJJOHOL4.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　切離４　】'
              GOTO ABEND END
    DACTLF FILE-DJJOHOL8.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　切離８　】'
              GOTO ABEND END
    CNDPF FILE-DJJOHOF.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　圧縮中　】'
              GOTO ABEND END
    ACTLF FILE-DJJOHOL1.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　関連１　】'
              GOTO ABEND END
    ACTLF FILE-DJJOHOL2.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　関連２　】'
              GOTO ABEND END
    ACTLF FILE-DJJOHOL3.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　関連３　】'
              GOTO ABEND END
    ACTLF FILE-DJJOHOL4.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　関連４　】'
              GOTO ABEND END
    ACTLF FILE-DJJOHOL8.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【基本情報　関連８　】'
              GOTO ABEND END

/.##実績連携ＤＴ削除##./
SFU0510B:

    ?STEP :=   'SFU0510B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績連携データ削除',TO-XCTL

    OVRF FILE-PCJISSL2,TOFILE-PCJISSL2.TOKKLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SFU0510B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【実績連携データ削除】'
              GOTO ABEND END

RTN:

    ?KBN   :=  ?OK
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KBN   :=  ?NG
    ?KEKA1 :=  '在庫更新（その他）が異常終了しま'
    ?KEKA2 :=  'した。この画面のリストを出力して下さい'
    ?KEKA3 :=  '（ログリストを採取しＮＡＶへ連絡）'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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