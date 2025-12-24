/. ************************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　        *  ./
/. *   SYSTEM-NAME :    苗業務システム　ＨＧ部連携　　　　    *  ./
/. *   JOB-ID      :    PNA04100                              *  ./
/. *   JOB-NAME    :    商品変換テーブル一覧表抽出（小売連携）*  ./
/. ************************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PNA04100'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?TOKCD  ,STRING*8,VALUE-'        '  /.バッチ時間./
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '苗業務：ＨＧ部連携　変換ＴＢＬ一覧出力'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##初期化##./
STEP010:

    ?STEP :=   'STEP010 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SHOTBLWK.TOKKLIB
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 :=  '【出力Ｆ初期化】'
         GOTO ABEND
    END

/.##範囲指定画面##./
STEP020:

    ?STEP :=   'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIBO
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL SNA0400I.TOKELIBO,PARA-(?TOKCD)
    IF   @PGMEC    ^=   0    THEN
         IF  @PGMEC  =  4010   THEN
             SNDMSG MSG-'＃取消終了＃',TO-XCTL
             RETURN
         ELSE
              ?KEKA4 :=  '【商品変換ＴＢＬ一覧　範囲指定】'
              GOTO ABEND
         END
    END
    ?MSGX :=  '## ﾊﾟﾗ取引先 = ' && ?TOKCD && ' ##'
    SNDMSG    ?MSGX,TO-XCTL

/.##初期化##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SHOTBLWK.TOKKLIB
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 :=  '【出力Ｆ初期化】'
         GOTO ABEND
    END

/.##商品変換ＴＢＬ一覧出力##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SHOTBLW1,TOFILE-SHOTBLW1.TOKKLIB
    OVRF FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL SNA0410B.TOKELIBO,PARA-(?TOKCD)
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 :=  '【商品変換ＴＢＬ一覧出力】'
         GOTO ABEND
    END

/.##ＣＳＶ出力##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVDF FILE-SHOTBLWK.TOKKLIB,PATH-'/HGNAS/SHOTBLWK.CSV',
          MODE-@EXP,ADD-@NO,ACTCHK-@NO,HEADLINE-@YES,
          EXTCNV-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 :=  '【ＣＳＶ出力】'
         GOTO ABEND
    END


/.##プログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '商品変換ＴＢＬ一覧(小売連携対象）の出力が'
    ?KEKA2 :=  '正常終了しました。ＣＳＶ出力内容を確認して'
    ?KEKA3 :=  '下さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '商品変換ＴＢＬ一覧（小売連携対象）の出力が'
    ?KEKA2 :=  '異常終了しました。詳細ログを出力し、ＮＡＶ'
    ?KEKA3 :=  '送付して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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