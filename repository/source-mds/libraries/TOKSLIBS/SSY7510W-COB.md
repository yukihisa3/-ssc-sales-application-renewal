# SSY7510W

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7510W.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭサンワ　　　　　　　　　　　*
*    モジュール名　　　　：　受領ＭＳＧ抽出　　　　　　　　　　*
*    作成日／更新日　　　：　2015/11/27                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　日付指定区分 PARA-DTKBN           *
*                        ：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-JYUDT           *
*                        ：　検収日開始   PARA-KENDTS          *
*                        ：　検収日終了   PARA-KENDTE          *
*    処理概要　　　　　　：　ＩＮＰＵＴパラメタより、受領ＭＳ  *
*                        ：　Ｇを抽出する。　　　　　　　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY7510W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/08/20.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 受領MSG L2 >>--*
     SELECT   JURYOL1   ASSIGN         DA-01-VI-SWJYURL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JU1-F03
                                       JU1-F01
                        STATUS         JURYOL1-ST.
*----<< 受領MSG L3 >>--*
     SELECT   JURYOL2   ASSIGN         DA-01-VI-SWJYURL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JU2-F03
                                       JU2-F05
                        STATUS         JURYOL2-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         HTENMS-ST.
*----<< 受領ワーク >>--*
     SELECT   SWJYRWF   ASSIGN    TO        DA-01-VS-SWJYRWF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   JWK-STATUS.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 受領MSG L2 >>--*
 FD  JURYOL1            LABEL     RECORD   IS   STANDARD.
     COPY     SWJYURF   OF        XFDLIB
              JOINING   JU1       PREFIX.
*----<< 受領MSG L3 >>--*
 FD  JURYOL2            LABEL     RECORD   IS   STANDARD.
     COPY     SWJYURF   OF        XFDLIB
              JOINING   JU2       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 受領ワーク >>--*
 FD  SWJYRWF            LABEL     RECORD   IS   STANDARD.
     COPY     SWJYRWF   OF        XFDLIB
              JOINING   JWK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< 受領ファイル >>--*
     COPY   SWJYURF  OF XFDLIB  JOINING   JUR  AS   PREFIX.
*
*----<< プログラム名 >>--*
 01  PG-ID             PIC  X(08)  VALUE "SSY7510W".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JURYOL1-ST        PIC  X(02).
 01  JURYOL2-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  JWK-STATUS        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  TOK-INVALID-FLG   PIC  X(01).
 01  TEN-INVALID-FLG   PIC  X(01).
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
*----<< 名称マスタ取得用 >>--*
 01  MEISHO-AREA.
     03  MEISHO-TORICD      PIC   9(08).
     03  MEISHO-KUBUN       PIC   X(06).
     03  MEISHO-CODE        PIC   X(10).
     03  MEISHO-MEI         PIC   N(20).
     03  MEISHO-KANA        PIC   X(20).
     03  MEISHO-KEKKA       PIC   X(01).
*
*----<< 固定項目 >>--*
 01  DEMPYO-KUBUN           PIC   X(06)  VALUE "DCM001".
*
*----<< ﾍﾝｽｳ >>--*
 01  WK-AREA.
     03  WK-DEMPYOKUBUN     PIC   N(20)  VALUE SPACE.
     03  WK-DEMPYOKUBUN-K   PIC   X(20)  VALUE SPACE.
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7510W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7510W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7510W".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 COPY     KEIJYRF   OF        XFDLIB.
*
 LINKAGE                SECTION.
 01  PARA-DTKBN             PIC   X(01).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-JYUDT             PIC   9(08).
 01  PARA-KENDTS            PIC   9(08).
 01  PARA-KENDTE            PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-DTKBN
                                        PARA-TORICD
                                        PARA-JYUDT
                                        PARA-KENDTS
                                        PARA-KENDTE.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 受領ＭＳＧＬ１ >>--*
 JURYOL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JURYOL1.
     MOVE      "SWJYURF"    TO   AB-FILE.
     MOVE      JURYOL1-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 受領ＭＳＧＬ２ >>--*
 JURYOL2-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JURYOL2.
     MOVE      "SWJYURF"    TO   AB-FILE.
     MOVE      JURYOL2-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     MOVE      "HTOKMS"     TO   AB-FILE.
     MOVE      HTOKMS-ST    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     MOVE      "HTENMS"     TO   AB-FILE.
     MOVE      HTENMS-ST    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 受領ワーク >>--*
 SWJYRWF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SWJYRWF.
     MOVE      "SWJYRWF"    TO   AB-FILE.
     MOVE      JWK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.

 END     DECLARATIVES.
****************************************************************
*                                                              *
****************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*----<< 開始メッセージ >>--*
     CALL    "SKYMSGS" USING     PG-ID.
*
*----<<FILE OPEN >>--*
     OPEN     INPUT     JURYOL1
                        JURYOL2.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    SWJYRWF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*----<< 取引先Ｍ読込 >>--*
     MOVE     PARA-TORICD     TO   TOK-F01.
     PERFORM  900-TOK-READ.
*
*----<< クリア >>--*
     MOVE     SPACE     TO        JU1-REC.
     INITIALIZE                   JU1-REC.
     MOVE     SPACE     TO        JU2-REC.
     INITIALIZE                   JU2-REC.
*
*----<< キー情報セット >>--*
     IF  PARA-DTKBN  =  "1"
       THEN
         MOVE     PARA-TORICD     TO   JU1-F03
         MOVE     PARA-JYUDT      TO   JU1-F01
       ELSE
         MOVE     PARA-TORICD     TO   JU2-F03
         MOVE     PARA-KENDTS     TO   JU2-F05
     END-IF.
*
*----<< FILE START >>--*
     IF  PARA-DTKBN  =  "1"
       THEN
         START    JURYOL1  KEY  >=   JU1-F03
                                     JU1-F01
           INVALID   KEY
             MOVE      9    TO   END-FG
               GO   TO   INIT-EXIT
         END-START
       ELSE
         START    JURYOL2  KEY  >=   JU2-F03
                                     JU2-F05
           INVALID   KEY
             MOVE      9    TO   END-FG
               GO   TO   INIT-EXIT
         END-START
     END-IF.
*
*----<< 1ｹﾝﾒ ﾖﾐｺﾐ >>--*
     IF  PARA-DTKBN  =  "1"
       THEN
         PERFORM  900-JU1-READ
       ELSE
         PERFORM  900-JU2-READ
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >>--*
     MOVE     SPACE     TO        JUR-REC.
     INITIALIZE                   JUR-REC.
     INITIALIZE                   WK-AREA.
     MOVE     SPACE     TO        JWK-REC.
     INITIALIZE                   JWK-REC.
*
*----<< 受領レコードをＷＫレコードへ >>--*
     IF  PARA-DTKBN  =  "1"
         MOVE     JU1-REC         TO   JUR-REC
       ELSE
         MOVE     JU2-REC         TO   JUR-REC
     END-IF.
*
*----<< 店舗Ｍ読込 >>--*
     MOVE     PARA-TORICD     TO   TEN-F52.
     MOVE     JUR-F04         TO   TEN-F011.
     PERFORM  900-TEN-READ.
*
*----<< 名称取得 >>--*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     DEMPYO-KUBUN    TO   MEISHO-KUBUN.
     MOVE     JUR-F207        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
*
*----<< ワーク作成 >>--*
     MOVE     PARA-TORICD    TO   JWK-F01.
     MOVE     TOK-F03        TO   JWK-F02.
     MOVE     JUR-F04        TO   JWK-F03.
     MOVE     TEN-F03        TO   JWK-F04.
     MOVE     JUR-F05        TO   JWK-F05.
     MOVE     JUR-F06        TO   JWK-F06.
     MOVE     JUR-F07        TO   JWK-F07.
     MOVE     JUR-F311       TO   JWK-F08.
     MOVE     JUR-F309       TO   JWK-F09.
     MOVE     JUR-F310       TO   JWK-F10.
     MOVE     JUR-F306       TO   JWK-F16.
     MOVE     JUR-F307       TO   JWK-F17.
     MOVE     JUR-F207       TO   JWK-F18.
     MOVE     MEISHO-MEI(1:4)
                             TO   JWK-F19.
     MOVE     PARA-DTKBN     TO   JWK-F96.
     MOVE     PARA-JYUDT     TO   JWK-F97.
     MOVE     PARA-KENDTS    TO   JWK-F98.
     MOVE     PARA-KENDTE    TO   JWK-F99.
*発注数量
     COMPUTE  JWK-F11        =    JUR-F312 / 10.
*検収数量
     COMPUTE  JWK-F12        =    JUR-F314 / 10.
*欠品数量
     COMPUTE  JWK-F13        =    JWK-F11 - JWK-F12
*原価単価
     COMPUTE  JWK-F14        =    JUR-F315 / 100.
*売価単価
     MOVE     JUR-F316       TO   JWK-F15.
*
*----<< ワーク書込 >>--*
     WRITE    JWK-REC.
     ADD      1              TO   WRT-CNT.
*
*----<< 次レコード読込 >>--*
     IF  PARA-DTKBN  =  "1"
       THEN
         PERFORM  900-JU1-READ
       ELSE
         PERFORM  900-JU2-READ
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
*
*----<< 終了メッセージ >>--*
     CALL    "SKYMSGE" USING     PG-ID.
*
*----<<FILE CLOSE >>--*
     CLOSE     JURYOL1.
     CLOSE     JURYOL2.
     CLOSE     HTOKMS.
     CLOSE     HTENMS.
     CLOSE     SWJYRWF.
*
     IF  OUT-CNT = ZERO
         MOVE      4001         TO   PROGRAM-STATUS
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    受領ＭＳＧＬ１　 READ                        *
*--------------------------------------------------------------*
 900-JU1-READ           SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "900-JU1-READ"      TO   S-NAME.
*
*----<< 読込 >>--*
     READ     JURYOL1
       AT END
           MOVE      9    TO   END-FG
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*
*----<< 範囲チェック(受信日) >>--*
     IF (JU1-F01 = PARA-JYUDT AND
         JU1-F03 = PARA-TORICD )
     THEN
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
     END-IF.
 900-JU1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    受領ＭＳＧＬ２　 READ                        *
*--------------------------------------------------------------*
 900-JU2-READ           SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "900-JU2-READ"      TO   S-NAME.
*
*----<< 読込 >>--*
     READ     JURYOL2
       AT END
           MOVE      9    TO   END-FG
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*
*----<< 範囲チェック(検収日範囲) >>--*
     IF (JU2-F05 >= PARA-KENDTS AND
         JU2-F05 <= PARA-KENDTE AND
         JU2-F03 =  PARA-TORICD )
     THEN
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
     END-IF.
 900-JU2-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS    INVALID
              MOVE  ALL NC"＊"         TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　 READ                            *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     "900-TEN-READ"      TO   S-NAME.
     READ     HTENMS    INVALID
              MOVE  ALL NC"＊"         TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    名称マスタサブルーチン                       *
*--------------------------------------------------------------*
 900-MEISHO-SUB         SECTION.
     CALL    "SED9001B" USING     MEISHO-TORICD
                                  MEISHO-KUBUN
                                  MEISHO-CODE
                                  MEISHO-MEI
                                  MEISHO-KANA
                                  MEISHO-KEKKA.
 900-MEISHO-SUB-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
