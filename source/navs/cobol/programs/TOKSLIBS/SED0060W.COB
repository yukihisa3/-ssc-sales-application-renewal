****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　ＥＤＩＣシステム構築
*    業務名　　　　　　　：　ＥＤＩシステム出荷データ作成
*    モジュール名　　　　：　出荷データ作成　　　
*    作成日／更新日　　　：　2015/09/14
*    作成者／更新者　　　：　NAV TAKAHASIH
*    処理概要　　　　　　：　受け取ったパラメタより対象データ
*                            を出荷データを抽出する。
*    更新履歴            ：
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SED0060W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/09/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*ＥＤＩＣ発注ＭＳＧ
     SELECT   EDJOHOF   ASSIGN    TO        DA-01-VI-EDJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F011  JOH-F012
                                            JOH-F013  JOH-F02
                                            JOH-F04   JOH-F03
                                            JOH-F05   JOH-F06
                        FILE      STATUS    JOH-STATUS.
*ＥＤＩＣ出荷ＭＳＧ
     SELECT   EDSYUKF   ASSIGN    TO        DA-01-VI-EDSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SYK-F011  SYK-F012
                                            SYK-F013  SYK-F02
                                            SYK-F03   SYK-F04
                                            SYK-F05   SYK-F06
                        FILE      STATUS    SYK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    ＥＤＩＣ発注ＭＳＧ
******************************************************************
 FD  EDJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     EDJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    ＥＤＩＣ出荷ＭＳＧ
******************************************************************
 FD  EDSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     EDSYUKF   OF        XFDLIB
              JOINING   SYK       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ワーク出荷情報データ
     COPY   EDSYUKF  OF XFDLIB  JOINING   SYW  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  HED-CNT             PIC  9(08)     VALUE  ZERO.
     03  MEI-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUTPUT-CNT          PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  EDSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-TEL                  PIC  X(12)     VALUE  "045-945-8816".
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*システム時刻の編集
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMMSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  SYK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0060W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0060W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0060W".
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
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT
                                       PARA-BUMON
                                       PARA-TANCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   EDJOHOF.
     MOVE      "EDJOHOL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   EDSYUKF.
     MOVE      "EDSYUKL2"   TO   AB-FILE.
     MOVE      SYK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     DISPLAY "SYK-F011 = " SYK-F011  UPON CONS.
     DISPLAY "SYK-F012 = " SYK-F012  UPON CONS.
     DISPLAY "SYK-F013 = " SYK-F013  UPON CONS.
     DISPLAY "SYK-F02  = " SYK-F02   UPON CONS.
     DISPLAY "SYK-F03  = " SYK-F03   UPON CONS.
     DISPLAY "SYK-F04  = " SYK-F04   UPON CONS.
     DISPLAY "SYK-F05  = " SYK-F05   UPON CONS.
     DISPLAY "SYK-F06  = " SYK-F06   UPON CONS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       EDJOHOF  SHTDENF.
     OPEN     I-O       EDSYUKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*システム時刻の取得
     ACCEPT   SYS-TIME       FROM TIME.
*    ＥＤＩＣ発注ＭＳＧスタート
     MOVE     SPACE          TO   JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE     PARA-JDATE     TO   JOH-F011.
     MOVE     PARA-JTIME     TO   JOH-F012.
     MOVE     PARA-TORICD    TO   JOH-F013.
     MOVE     PARA-SOKO      TO   JOH-F02.
     MOVE     PARA-NOUDT     TO   JOH-F04.
     MOVE     ZERO           TO   JOH-F03.
     MOVE     ZERO           TO   JOH-F05.
     MOVE     ZERO           TO   JOH-F06.
     START    EDJOHOF   KEY  >=   JOH-F011  JOH-F012
                                  JOH-F013  JOH-F02
                                  JOH-F04   JOH-F03
                                  JOH-F05   JOH-F06
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＥＤＩＣ発注ＭＳＧデータ読込み
     PERFORM EDJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDJOHOF-READ-SEC    SECTION.
*
     READ     EDJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  EDJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
 READ-010.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  JOH-F011
     AND      PARA-JTIME  =  JOH-F012
     AND      PARA-TORICD =  JOH-F013
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  EDJOHOF-READ-EXIT
     END-IF.
 READ-020.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  JOH-F013
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  EDJOHOF-READ-EXIT
              END-IF
     END-IF.
 READ-030.
*    送信ＦＬＧのチェック
     IF       JOH-F801  =  "1"
              GO                 TO   EDJOHOF-READ-SEC
     END-IF.
 READ-040.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  JOH-F02
                   CONTINUE
              ELSE
                   GO            TO  EDJOHOF-READ-SEC
              END-IF
     END-IF.
 READ-050.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  JOH-F04
                  CONTINUE
              ELSE
                  GO        TO   EDJOHOF-READ-SEC
              END-IF
     END-IF.
*
 EDJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     JOH-F013            TO   DEN-F01.
     MOVE     JOH-F05             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     JOH-F06             TO   DEN-F03.
     MOVE     JOH-F03             TO   DEN-F07.  *> 店舗CD
     MOVE     JOH-F04             TO   DEN-F112. *> 納入日
     READ     SHTDENF    INVALID
              MOVE    "INV"       TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENF-INV-FLG
     END-READ.
*
     IF       SHTDENF-INV-FLG  =  "INV"
              ADD     1           TO   SKIP2-CNT
******        DISPLAY "AAAAA" UPON CONS
              GO                  TO   MAIN010
     ELSE
**************数量確認(発注数<>出荷数の時ﾌｧｲﾙに出力)
*             PERFORM   EDSYUKF-READ-SEC
*             IF    EDSYUKF-INV-FLG = "INV"
                    PERFORM   EDSYUKF-WRITE-SEC
*****               MOVE   "1"       TO   JOH-F801
*****               MOVE  SYS-DATEW  TO   JOH-F802
*****               MOVE  SYS-HHMMSS TO   JOH-F803
                    REWRITE  JOH-REC
*             END-IF
     END-IF.
 MAIN010.
*    ＥＤＩＣ発注ＭＳＧデータ読込み
     PERFORM EDJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　リック　ファイルヘッダー、出荷ヘッダ１，２セット
****************************************************************
 KOUMOKU-SET-SEC       SECTION.
*
     MOVE      SPACE               TO  SYK-REC.
     INITIALIZE                        SYK-REC.
*
     MOVE      JOH-F011            TO  SYK-F011.
     MOVE      JOH-F012            TO  SYK-F012.
     MOVE      JOH-F013            TO  SYK-F013.
     MOVE      JOH-F02             TO  SYK-F02.
     MOVE      JOH-F03             TO  SYK-F03.
     MOVE      JOH-F04             TO  SYK-F04.
     MOVE      JOH-F05             TO  SYK-F05.
     MOVE      ZERO                TO  SYK-F06.
*ファイルヘッダーセット
     MOVE      JOH-F100            TO  SYK-F100.
     MOVE      "2000"              TO  SYK-F101.
     MOVE      "A"                 TO  SYK-F102.
     MOVE      "MSG-SHIP"          TO  SYK-F103.
     MOVE      "0000000000001"     TO  SYK-F104.
     MOVE      "0000000000001"     TO  SYK-F105.
     MOVE "ｶﾌﾞｼｷｶﾞｲｼｬﾋﾞｰﾊﾞｰﾄｻﾞﾝ"   TO  SYK-F106.
*ヘッダー１転送
     MOVE      "2010"              TO  SYK-F201.
     MOVE      "MSG-SHIP"          TO  SYK-F202.
     MOVE      JOH-F203            TO  SYK-F203.
     MOVE      JOH-F204            TO  SYK-F204.
     MOVE      JOH-F205            TO  SYK-F205.
     MOVE      JOH-F206            TO  SYK-F206.
     MOVE      JOH-F207            TO  SYK-F207.
     MOVE      JOH-F208            TO  SYK-F208.
     MOVE      JOH-F209            TO  SYK-F209.
     MOVE      JOH-F210            TO  SYK-F210.
     MOVE      JOH-F211            TO  SYK-F211.
     MOVE      JOH-F212            TO  SYK-F212.
     MOVE      JOH-F213            TO  SYK-F213.
     MOVE      JOH-F214            TO  SYK-F214.
     MOVE      JOH-F215A           TO  SYK-F215A.
     MOVE      JOH-F215            TO  SYK-F215.
     MOVE      JOH-F215B           TO  SYK-F215B.
     MOVE      JOH-F216            TO  SYK-F216.
     MOVE      JOH-F217            TO  SYK-F217.
     MOVE      JOH-F218            TO  SYK-F218.
     MOVE      JOH-F219            TO  SYK-F219.
     MOVE      JOH-F220            TO  SYK-F220.
     MOVE      JOH-F221            TO  SYK-F221.
     MOVE      JOH-F222            TO  SYK-F222.
     MOVE      JOH-F223            TO  SYK-F223.
     MOVE      JOH-F224            TO  SYK-F224.
     MOVE      JOH-F225            TO  SYK-F225.
     MOVE      "ｻｶﾀﾉﾀﾈ"            TO  SYK-F226.
     MOVE      WK-TEL              TO  SYK-F227.
     MOVE      JOH-F227            TO  SYK-F228.
     MOVE      JOH-F228            TO  SYK-F229.
     MOVE      JOH-F229            TO  SYK-F230.
     MOVE      JOH-F230            TO  SYK-F231.
*ヘッダー２転送
     MOVE      "2011"              TO  SYK-F301.
     MOVE      JOH-F302            TO  SYK-F302.
     MOVE      JOH-F303            TO  SYK-F303.
     MOVE      JOH-F304            TO  SYK-F304.
     MOVE      JOH-F305            TO  SYK-F305.
     MOVE      JOH-F306            TO  SYK-F306.
*****MOVE      DEN-F112            TO  SYK-F307.
     MOVE      DEN-F112            TO  WK-DEN-F112.
*****納品日の－１の日付をセット
     MOVE      "6"                 TO  LINK-IN-KBN.
     MOVE       1                  TO  LINK-IN-YMD6.
     MOVE      WK-DEN-F112         TO  LINK-IN-YMD8.
     CALL      "SKYDTCKB" USING        LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE    LINK-OUT-YMD8    TO   SYK-F307
     ELSE
              DISPLAY NC"出荷日算出エラー！"     UPON CONS
              DISPLAY NC"算出元納品日＝" WK-DEN-F112 UPON CONS
              DISPLAY NC"エラーＣＤ　＝" LINK-OUT-RET
                                                 UPON CONS
              MOVE    WK-DEN-F112      TO   SYK-F307
     END-IF.
     MOVE      ZERO                TO  SYK-F308.
     MOVE      ZERO                TO  SYK-F309.
     MOVE      ZERO                TO  SYK-F310.
     MOVE      JOH-F307            TO  SYK-F311.
     MOVE      JOH-F308            TO  SYK-F312.
     MOVE      JOH-F309            TO  SYK-F313.
     MOVE      "01"                TO  SYK-F314.
     MOVE      JOH-F310            TO  SYK-F315.
     MOVE      JOH-F311            TO  SYK-F316.
     MOVE      JOH-F312            TO  SYK-F317.
     COMPUTE   SYK-F318  =  DEN-F181  *  100.
     COMPUTE   SYK-F319  =  DEN-F182  *  100.
*****MOVE      DEN-F181            TO  SYK-F318.
*****MOVE      DEN-F182            TO  SYK-F319.
     MOVE      SYK-F319            TO  SYK-F320.
     COMPUTE   SYK-F321  =  DEN-F15  *  10.
*****MOVE      DEN-F15             TO  SYK-F321.
     MOVE      JOH-F318            TO  SYK-F322.
     MOVE      ZERO                TO  SYK-F323.
     MOVE      ZERO                TO  SYK-F324.
*
 KOUMOKU-SET-EXIT.
     EXIT.
****************************************************************
*　　リック　発注確定データ作成処理
****************************************************************
 EDSYUKF-WRITE-SEC     SECTION.
*
     MOVE     "EDSYUKF-WRITE-SEC"  TO  S-NAME.
*
     MOVE      SPACE               TO  SYK-REC.
 EDSYUKF-010.
*****DISPLAY "JOH-F06 = " JOH-F06.
     INITIALIZE                        SYK-REC.
*行番号＝０のレコードを作成する。
*　ファイルヘッダ、出荷ヘッダ１，２情報をセット
     MOVE      JOH-F011            TO  SYK-F011.
     MOVE      JOH-F012            TO  SYK-F012.
     MOVE      JOH-F013            TO  SYK-F013.
     MOVE      JOH-F02             TO  SYK-F02.
     MOVE      JOH-F03             TO  SYK-F03.
     MOVE      JOH-F04             TO  SYK-F04.
     MOVE      JOH-F05             TO  SYK-F05.
     MOVE      ZERO                TO  SYK-F06.
 EDSYUKF-020.
*行＝０レコード検索
     PERFORM   EDSYUKF-READ-SEC.
 EDSYUKF-030.
*行＝０が存在チェック
     IF  EDSYUKF-INV-FLG  =  "INV"
*********行＝０が存在しない場合
         PERFORM  KOUMOKU-SET-SEC
         MOVE     SYK-REC          TO  SYW-REC
         WRITE    SYK-REC
         ADD      1                TO  HED-CNT  OUTPUT-CNT
     ELSE
         MOVE     SYK-REC          TO  SYW-REC
         COMPUTE   SYK-F318  =  SYK-F318 + (DEN-F181  *  100)
         COMPUTE   SYK-F319  =  SYK-F319 + (DEN-F182  *  100)
*****    COMPUTE   SYK-F318  =  SYK-F318 +  DEN-F181
*****    COMPUTE   SYK-F319  =  SYK-F319 +  DEN-F182
         MOVE      SYK-F319        TO  SYK-F320
         COMPUTE   SYK-F321  =  SYK-F321 + (DEN-F15   *  10 )
*****    COMPUTE   SYK-F321  =  SYK-F321 +  DEN-F15
         REWRITE   SYK-REC
     END-IF.
 EDSYUKF-040.
*
*行番号のレコードを作成する。
*　ファイルヘッダ、出荷ヘッダ１，２情報をセット
     MOVE      JOH-F011            TO  SYK-F011.
     MOVE      JOH-F012            TO  SYK-F012.
     MOVE      JOH-F013            TO  SYK-F013.
     MOVE      JOH-F02             TO  SYK-F02.
     MOVE      JOH-F03             TO  SYK-F03.
     MOVE      JOH-F04             TO  SYK-F04.
     MOVE      JOH-F05             TO  SYK-F05.
     MOVE      JOH-F06             TO  SYK-F06.
 EDSYUKF-050.
*行＝０レコード検索
     PERFORM   EDSYUKF-READ-SEC.
 EDSYUKF-060.
*行＝０が存在チェック
     IF  EDSYUKF-INV-FLG  =  "INV"
         MOVE  SPACE               TO  SYK-REC
         INITIALIZE                    SYK-REC
         MOVE  SYW-REC             TO  SYK-REC
         MOVE  JOH-F06             TO  SYK-F06
*********出荷明細転送
         MOVE  "2020"              TO  SYK-F401
         MOVE  JOH-F402            TO  SYK-F402
         MOVE  JOH-F403            TO  SYK-F403
         MOVE  JOH-F404            TO  SYK-F404
         MOVE  JOH-F405            TO  SYK-F405
         MOVE  JOH-F406            TO  SYK-F406
         MOVE  JOH-F407            TO  SYK-F407
         MOVE  JOH-F408            TO  SYK-F408
         MOVE  JOH-F409            TO  SYK-F409
         MOVE  JOH-F410            TO  SYK-F410
         MOVE  ZERO                TO  SYK-F411
         MOVE  JOH-F411A           TO  SYK-F412A
         MOVE  JOH-F411            TO  SYK-F412
         MOVE  JOH-F411B           TO  SYK-F412B(1:1)
         MOVE  JOH-F411C           TO  SYK-F412B(2:1)
         MOVE  JOH-F412            TO  SYK-F413
         MOVE  JOH-F413A           TO  SYK-F414A
         MOVE  JOH-F413            TO  SYK-F414
         MOVE  JOH-F413B           TO  SYK-F414B(1:1)
         MOVE  JOH-F413C           TO  SYK-F414B(2:1)
         MOVE  JOH-F414            TO  SYK-F415
         MOVE  JOH-F415            TO  SYK-F416
         MOVE  JOH-F416            TO  SYK-F417
         MOVE  JOH-F417            TO  SYK-F418
         COMPUTE  SYK-F419  =  DEN-F181  *  100
*****    MOVE  DEN-F181            TO  SYK-F419
         MOVE  JOH-F419            TO  SYK-F420
         COMPUTE  SYK-F421  =  DEN-F182  *  100
*****    MOVE  DEN-F182            TO  SYK-F421
         MOVE  JOH-F421            TO  SYK-F422
         MOVE  JOH-F422            TO  SYK-F423
         MOVE  JOH-F423            TO  SYK-F424
         MOVE  JOH-F424            TO  SYK-F425
         MOVE  JOH-F425            TO  SYK-F426
         COMPUTE  SYK-F427  =  DEN-F15   *  10
         IF   DEN-F15  NOT =  DEN-F50
              COMPUTE  SYK-F428  =  (DEN-F50 - DEN-F15) * 10
*****         COMPUTE  SYK-F428  =  (DEN-F50 - DEN-F15)
              MOVE  "21"           TO  SYK-F429
         ELSE
              MOVE  ZERO           TO  SYK-F428
              MOVE  "00"           TO  SYK-F429
         END-IF
         WRITE  SYK-REC
         ADD    1                  TO  MEI-CNT   OUTPUT-CNT
     ELSE
         ADD    1                  TO  SKIP-CNT
     END-IF.
*
 EDSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "EDJOHOF READ CNT     = " READ-CNT  UPON CONS.
     DISPLAY "SHTDENF INV  CNT     = " SKIP2-CNT UPON CONS.
     DISPLAY "HEAD    OUT  CNT     = " HED-CNT   UPON CONS.
     DISPLAY "MEISAI  OUT  CNT     = " MEI-CNT   UPON CONS.
     DISPLAY "OUTPUT  OUT  CNT     = " OUTPUT-CNT   UPON CONS.
*
     CLOSE     SHTDENF  EDJOHOF  EDSYUKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDSYUKF-READ-SEC    SECTION.
*
*    MOVE     SPACE          TO   SYK-REC.
*    INITIALIZE                   SYK-REC.
*    MOVE     JOH-F011       TO   SYK-F011.
*    MOVE     JOH-F012       TO   SYK-F012
*    MOVE     JOH-F013       TO   SYK-F013.
*    MOVE     JOH-F02        TO   SYK-F02.
*    MOVE     JOH-F03        TO   SYK-F03.
*    MOVE     JOH-F04        TO   SYK-F04.
*    MOVE     JOH-F05        TO   SYK-F05.
*    MOVE     JOH-F06        TO   SYK-F06.
     READ     EDSYUKF    INVALID
              MOVE    "INV"       TO   EDSYUKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   EDSYUKF-INV-FLG
     END-READ.
*
 EDSYUKF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
