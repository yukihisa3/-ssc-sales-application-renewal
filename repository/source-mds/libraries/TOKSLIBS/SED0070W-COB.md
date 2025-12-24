# SED0070W

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SED0070W.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　ＥＤＩＣシステム構築
*    業務名　　　　　　　：　ＥＤＩシステム出荷データ作成
*    モジュール名　　　　：　出荷データ作成　　　
*    作成日／更新日　　　：　2015/09/15
*    作成者／更新者　　　：　NAV TAKAHASIH
*    処理概要　　　　　　：　受け取ったパラメタより対象データ
*                            を出荷データを抽出する。
*    更新履歴            ：
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SED0070W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/09/15.
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
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*ＥＤＩＣ出荷情報ファイル
     SELECT   EDSYUKF   ASSIGN    TO        DA-01-VI-EDSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SYK-F011  SYK-F012
                                            SYK-F013  SYK-F02
                                            SYK-F03   SYK-F04
                                            SYK-F05   SYK-F06
                        FILE      STATUS    SYK-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*店舗マスタ
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-STATUS.
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
*    ＥＤＩＣ出荷情報データ
******************************************************************
 FD  EDSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     EDSYUKF   OF        XFDLIB
              JOINING   SYK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
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
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-HEN-13               PIC  9(13)     VALUE  ZERO.
 01  WK-HEN-10               PIC  9(10)     VALUE  ZERO.
 01  WK-HEN-4                PIC  9(04)     VALUE  ZERO.
 01  WK-TEL                  PIC  X(12)  VALUE  "045-945-8816".
 01  WK-SYUKA-DATE           PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*システム時刻の編集
 01  SYS-TIME              PIC  9(08).
 01  FILLER                REDEFINES      SYS-TIME.
     03  SYS-HHMMSS        PIC  9(06).
     03  SYS-MS            PIC  9(02).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SYK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0070W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0070W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0070W".
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
 01  PARA-TORICD            PIC   9(08).
 01  PARA-DENST             PIC   9(09).
 01  PARA-DENED             PIC   9(09).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TORICD
                                       PARA-DENST
                                       PARA-DENED
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
                        PROCEDURE   EDSYUKF.
     MOVE      "EDSYUKL1"   TO   AB-FILE.
     MOVE      SYK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     MOVE      "HTENMS"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       SHTDENF.
     OPEN     I-O       EDSYUKF.
     OPEN     INPUT     SHOTBL1.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     HTENMS.
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
*システム時刻取得
     ACCEPT      SYS-TIME  FROM      TIME.
*    売上伝票データスタート
     MOVE      SPACE          TO   DEN-REC
     INITIALIZE                    DEN-REC
     MOVE      PARA-TORICD    TO   DEN-F01
     MOVE      PARA-DENST     TO   DEN-F02
     START     SHTDENF   KEY  >=   DEN-F01   DEN-F02
                                   DEN-F04   DEN-F051
***2011.10.06(DEN-F07,DEN-F112)
                                   DEN-F07   DEN-F112
                                   DEN-F03
          INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    リック　売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読込　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC    SECTION.
*
     READ     SHTDENF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  SHTDENF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    取引先のチェック
     IF       PARA-TORICD  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-TORICD  =  DEN-F01
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  SHTDENF-READ-EXIT
              END-IF
     END-IF.
*    伝票番号のチェック
     IF       PARA-DENST   =  ZERO
              CONTINUE
     ELSE
              IF   PARA-DENST   <=  DEN-F02    AND
                   DEN-F02      <=  PARA-DENED
                   CONTINUE
              ELSE
                   MOVE     "END"        TO  END-FLG
                   GO                    TO  SHTDENF-READ-EXIT
              END-IF
     END-IF.
*オンラインの場合は対象外とする。
     IF       DEN-F274  NOT =  ZERO
              GO                 TO   SHTDENF-READ-SEC
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-SOKO  =  DEN-F09
                   CONTINUE
              ELSE
                   ADD    1      TO  SKIP2-CNT
                   GO            TO  SHTDENF-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-NOUDT  =  DEN-F112
                  CONTINUE
              ELSE
                  ADD  1    TO   SKIP2-CNT
                  GO        TO   SHTDENF-READ-SEC
              END-IF
     END-IF.
*    DEN-F03=80 YOMITOBASI
     IF       DEN-F03 = 80
              ADD  1    TO   SKIP2-CNT
              GO        TO   SHTDENF-READ-SEC
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*MAIN010.
*    リック 出荷情報データWRITE
     PERFORM   EDSYUKF-WRITE-SEC.
*
*    リック 売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
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
     MOVE      99999999            TO  SYK-F011.
     MOVE      9999                TO  SYK-F012.
     MOVE      PARA-TORICD         TO  SYK-F013.
     MOVE      DEN-F08             TO  SYK-F02.
     MOVE      DEN-F07             TO  SYK-F03.
     MOVE      DEN-F112            TO  SYK-F04.
     MOVE      DEN-F02             TO  SYK-F05.
     MOVE      ZERO                TO  SYK-F06.
*ファイルヘッダーセット
     MOVE      "2000"              TO  SYK-F101.
     MOVE      "A"                 TO  SYK-F102.
     MOVE      "MSG-SHIP"          TO  SYK-F103.
     MOVE      "0000000000002"     TO  SYK-F104.
     MOVE      "0000000000002"     TO  SYK-F105.
     EVALUATE  DEN-F01
         WHEN  6002
               MOVE "ｶﾌﾞｼｷｶﾞｲｼｬﾋﾞｰﾊﾞｰﾄｻﾞﾝ"   TO  SYK-F106
         WHEN  120020
               MOVE "ｶﾀｸﾗｺｳｷﾞｮｳｶﾌﾞｼｶﾞｲｼｬ "   TO  SYK-F106
         WHEN  275
               MOVE "ｶﾌﾞｼｷｶﾞｲｼｬﾏｷﾊﾞ      "   TO  SYK-F106
         WHEN  OTHER
               MOVE "********************"   TO  SYK-F106
     END-EVALUATE.
     MOVE      DEN-F01             TO  WK-HEN-13.
     MOVE      WK-HEN-13           TO  SYK-F107.
     MOVE      WK-HEN-13           TO  SYK-F108.
     MOVE "ｶﾌﾞｼｷｶﾞｲｼｬｻｶﾀﾉﾀﾈHGﾌﾞ"   TO  SYK-F109.
     MOVE      SYS-DATEW           TO  SYK-F110.
     MOVE      SYS-HHMMSS          TO  SYK-F111.
     MOVE      ZERO                TO  SYK-F112.
     MOVE      SPACE               TO  SYK-F113.
*ヘッダー１転送
     MOVE      "2010"              TO  SYK-F201.
     MOVE      "MSG-SHIP"          TO  SYK-F202.
     MOVE      "0000000000002"     TO  SYK-F203.
     MOVE      "0000000000002"     TO  SYK-F204.
     MOVE      "0000000000002"     TO  SYK-F205.
     MOVE      "0000000000002"     TO  SYK-F206.
     EVALUATE  DEN-F01
         WHEN  6002
               MOVE "ｶﾌﾞｼｷｶﾞｲｼｬﾋﾞｰﾊﾞｰﾄｻﾞﾝ"   TO  SYK-F207
         WHEN  120020
               MOVE "ｶﾀｸﾗｺｳｷﾞｮｳｶﾌﾞｼｶﾞｲｼｬ "   TO  SYK-F207
         WHEN  275
               MOVE "ｶﾌﾞｼｷｶﾞｲｼｬﾏｷﾊﾞ      "   TO  SYK-F207
         WHEN  OTHER
               MOVE "********************"   TO  SYK-F207
     END-EVALUATE.
     MOVE      DEN-F02             TO  WK-HEN-10.
     MOVE      WK-HEN-10           TO  SYK-F208.
     MOVE      "0000000000"        TO  SYK-F209.
     MOVE      DEN-F07             TO  WK-HEN-13.
     MOVE      WK-HEN-13           TO  SYK-F210.
     MOVE      "0000000000001"     TO  SYK-F211.
**店舗マスタ索引
     MOVE      DEN-F07             TO  TEN-F011.
     MOVE      DEN-F01             TO  TEN-F52.
     READ      HTENMS    INVALID
                         MOVE      SPACE     TO   TEN-F02
                         MOVE      SPACE     TO   TEN-F03
     END-READ.
     MOVE      TEN-F04             TO  SYK-F212.
     MOVE      DEN-F07             TO  WK-HEN-13.
     MOVE      WK-HEN-13           TO  SYK-F213.
     MOVE      "0000000000001"     TO  SYK-F214.
     MOVE      X"28"               TO  SYK-F215A.
     MOVE      TEN-F03             TO  SYK-F215.
     MOVE      X"29"               TO  SYK-F215B.
     MOVE      TEN-F04             TO  SYK-F216.
     MOVE      SYK-F213            TO  SYK-F217.
     MOVE      "0000000000000"     TO  SYK-F218.
     MOVE      SPACE               TO  SYK-F219.
     MOVE      SPACE               TO  SYK-F220.
     MOVE      DEN-F01             TO  WK-HEN-13.
     MOVE      WK-HEN-13           TO  SYK-F221  SYK-F222.
     MOVE "ｶﾌﾞｼｷｶﾞｲｼｬｻｶﾀﾉﾀﾈHGﾌﾞ"   TO  SYK-F223.
     MOVE      WK-HEN-13           TO  SYK-F224  SYK-F225.
     MOVE "HG-ﾀﾝﾄｳ"                TO  SYK-F226.
     MOVE      WK-TEL              TO  SYK-F227.
     MOVE      "01"                TO  SYK-F228.
     MOVE      "00"                TO  SYK-F229.
     MOVE      "00"                TO  SYK-F230.
     MOVE      "00"                TO  SYK-F231.
*
*ヘッダー２転送
     MOVE      "2011"              TO  SYK-F301.
     MOVE      DEN-F12(1:2)        TO  SYK-F302(9:2)
     MOVE      "00000000"          TO  SYK-F302(1:8)
     MOVE      DEN-F12(3:2)        TO  SYK-F303(9:2)
     MOVE      "00000000"          TO  SYK-F303(1:8)
     MOVE      "00000000"          TO  SYK-F304.
     MOVE      "00000000"          TO  SYK-F305.
     MOVE      "00000000"          TO  SYK-F306.
     MOVE      DEN-F112            TO  WK-SYUKA-DATE.
     MOVE      WK-SYUKA-DATE       TO  SYK-F307.
     MOVE      ZERO                TO  SYK-F308.
     MOVE      ZERO                TO  SYK-F309.
     MOVE      ZERO                TO  SYK-F310.
     MOVE      "01"                TO  SYK-F311.
     MOVE      "05"                TO  SYK-F312.
     MOVE      "01"                TO  SYK-F313.
     MOVE      "02"                TO  SYK-F314.
     MOVE      "01"                TO  SYK-F315.
     MOVE      "02"                TO  SYK-F316.
     MOVE      "05"                TO  SYK-F317.
     COMPUTE   SYK-F318  =  DEN-F181  *  100.
     COMPUTE   SYK-F319  =  DEN-F182  *  100.
     MOVE      SYK-F319            TO  SYK-F320.
     COMPUTE   SYK-F321  =  DEN-F15  *  10.
     MOVE      "0000000000"        TO  SYK-F322.
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
 EDSYUKF-010.
     MOVE      SPACE               TO  SYK-REC.
     INITIALIZE                        SYK-REC.
*行番号＝０のレコードを作成する。
*　ファイルヘッダ、出荷ヘッダ１，２情報をセット
     MOVE      99999999            TO  SYK-F011.
     MOVE      9999                TO  SYK-F012.
     MOVE      DEN-F01             TO  SYK-F013.
     MOVE      DEN-F08             TO  SYK-F02.
     MOVE      DEN-F07             TO  SYK-F03.
     MOVE      DEN-F112            TO  SYK-F04.
     MOVE      DEN-F02             TO  SYK-F05.
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
         MOVE      SYK-F319        TO  SYK-F320
         COMPUTE   SYK-F321  =  SYK-F321 + (DEN-F15   *  10 )
         REWRITE   SYK-REC
     END-IF.
 EDSYUKF-040.
*
*行番号のレコードを作成する。
*　ファイルヘッダ、出荷ヘッダ１，２情報をセット
     MOVE      99999999            TO  SYK-F011.
     MOVE      9999                TO  SYK-F012.
     MOVE      DEN-F01             TO  SYK-F013.
     MOVE      DEN-F08             TO  SYK-F02.
     MOVE      DEN-F07             TO  SYK-F03.
     MOVE      DEN-F112            TO  SYK-F04.
     MOVE      DEN-F02             TO  SYK-F05.
     MOVE      DEN-F03             TO  SYK-F06.
 EDSYUKF-050.
*行が０以外のレコード検索
     PERFORM   EDSYUKF-READ-SEC.
 EDSYUKF-060.
*行＝０が存在チェック
     IF  EDSYUKF-INV-FLG  =  "INV"
         MOVE  SPACE               TO  SYK-REC
         INITIALIZE                    SYK-REC
         MOVE  SYW-REC             TO  SYK-REC
         MOVE  DEN-F03             TO  SYK-F06
*********出荷明細転送
         MOVE  "2020"              TO  SYK-F401
         MOVE  DEN-F03             TO  WK-HEN-4
         MOVE  WK-HEN-4            TO  SYK-F402
         MOVE  SPACE               TO  SYK-F403
         MOVE  "0000000001"        TO  SYK-F404
         MOVE  "0000000001"        TO  SYK-F405
         MOVE  "05"                TO  SYK-F406
         MOVE  "0"                 TO  SYK-F407(1:1)
         MOVE  DEN-F25             TO  SYK-F407(2:13)
         MOVE  "0"                 TO  SYK-F408(1:1)
         MOVE  DEN-F25             TO  SYK-F408(2:13)
         MOVE  SPACE               TO  SYK-F409
         MOVE  "005"               TO  SYK-F410
         MOVE  ZERO                TO  SYK-F411
*  商品名称マスタ検索
         MOVE        SPACE     TO        MEI-REC
         INITIALIZE                      MEI-REC
         MOVE  DEN-F1411       TO        MEI-F011
         MOVE  DEN-F1412       TO        MEI-F012
         READ    MEIMS1
                 INVALID
                 MOVE X"28"           TO  SYK-F412A
                 MOVE MEI-F021(1:12)  TO  SYK-F412
                 MOVE X"29"           TO  SYK-F412B(1:1)
                 MOVE SPACE           TO  SYK-F412B(2:1)
                 MOVE DEN-F1421       TO SYK-F413(1:15)
                 MOVE DEN-F1422(1:10) TO SYK-F413(16:10)
                 MOVE DEN-F1422(11:5) TO SYK-F415
                 MOVE X"28"           TO  SYK-F414A
                 MOVE MEI-F021(13:3)  TO  SYK-F414(1:3)
                 MOVE MEI-F022(1:9)   TO  SYK-F414(1:9)
                 MOVE X"29"           TO  SYK-F414B(1:1)
                 MOVE SPACE           TO  SYK-F414B(2:1)
                 NOT INVALID
                 MOVE X"28"           TO  SYK-F412A
                 MOVE MEI-F021(1:12)  TO  SYK-F412
                 MOVE X"29"           TO  SYK-F412B(1:1)
                 MOVE SPACE           TO  SYK-F412B(2:1)
                 MOVE DEN-F1421       TO SYK-F413(1:15)
                 MOVE DEN-F1422(1:10) TO SYK-F413(16:10)
                 MOVE DEN-F1422(11:5) TO SYK-F415
                 MOVE X"28"           TO  SYK-F414A
                 MOVE MEI-F021(13:3)  TO  SYK-F414(1:3)
                 MOVE MEI-F022(1:9)   TO  SYK-F414(1:9)
                 MOVE X"29"           TO  SYK-F414B(1:1)
                 MOVE SPACE           TO  SYK-F414B(2:1)
         END-READ
         MOVE  SPACE               TO  SYK-F416
         MOVE  SPACE               TO  SYK-F417
         COMPUTE  SYK-F418  =  DEN-F172  *  100
         COMPUTE  SYK-F419  =  DEN-F181  *  100
         COMPUTE  SYK-F420  =  DEN-F173  *  100
         COMPUTE  SYK-F421  =  DEN-F182  *  100
         MOVE  "0000000"           TO  SYK-F422
         MOVE  "0000"              TO  SYK-F423
         MOVE  "000000"            TO  SYK-F424
         MOVE  "00"                TO  SYK-F425
         MOVE  "00"                TO  SYK-F426
         COMPUTE  SYK-F427  =  DEN-F15   *  10
         MOVE  ZERO                TO  SYK-F428
         MOVE  "00"                TO  SYK-F429
         WRITE  SYK-REC
         ADD    1                  TO  MEI-CNT   OUTPUT-CNT
     ELSE
         ADD    1                  TO  SKIP-CNT
     END-IF.
*
 EDSYUKF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDSYUKF-READ-SEC    SECTION.
*
     READ     EDSYUKF    INVALID
              MOVE    "INV"       TO   EDSYUKF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   EDSYUKF-INV-FLG
     END-READ.
*
 EDSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "SHTDENF READ CNT     = " READ-CNT  UPON CONS.
     DISPLAY "SHTDENF SKIP CNT     = " SKIP2-CNT UPON CONS.
     DISPLAY "EDSYUKF SKIP CNT     = " SKIP-CNT  UPON CONS.
     DISPLAY "HEAD    OUT  CNT     = " HED-CNT   UPON CONS.
     DISPLAY "MEISAI  OUT  CNT     = " MEI-CNT   UPON CONS.
     DISPLAY "OUTPUT  OUT  CNT     = " OUTPUT-CNT   UPON CONS.
*
     CLOSE     SHTDENF  EDSYUKF  SHOTBL1  MEIMS1  HTENMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
