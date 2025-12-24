# SKY6000B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKY6000B.COB`

## ソースコード

```cobol
****************************************************************
*         SYSTEM･･･ジョイ物品受領書データ作成　　　　　        *
*        PG-NAME･･･物品受領書データ作成                        *
*          PG-ID･･･SKY6000B                                    *
*                                            DATE. 08.02.04    *
*                                              BY. NAV         *
*         UPDATE･･･基幹サーバ統合                              *
*                                            11.11.28/YOSHIDA.M*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY6000B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/02/04.
*REMARKS.
*
******************************************************************
*                                                                *
 ENVIRONMENT            DIVISION.
*                                                                *
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    ジョイ受領データ
     SELECT   JOYJYR             ASSIGN    TO   DA-01-S-JOYJYR
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   JOY-STATUS.
*    商品変換テーブル　
     SELECT   HSHOTBL            ASSIGN    TO   DA-01-VI-SHOTBL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SHO-F01  SHO-F02
                                 STATUS         SHO-STATUS.
*    商品名称マスタ　　
     SELECT   HMEIMS             ASSIGN    TO   DA-01-VI-MEIMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    MEI-F011 MEI-F0121
                                                MEI-F0122
                                                MEI-F0123
                                 STATUS         MEI-STATUS.
*    売上伝票ファイル　
     SELECT   SHTDENF            ASSIGN    TO   DA-01-VI-SHTDENL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SHT-F01  SHT-F02
                                                SHT-F04  SHT-F051
***2011.11.28 ST
                                                SHT-F07  SHT-F112
***2011.11.28 EN
                                                SHT-F03
                                 STATUS         SHT-STATUS.
*    店舗マスタ　　　　
     SELECT   HTENMS             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*    受領データ　　　　
     SELECT   JYURYOF            ASSIGN    TO   DA-01-VI-JYURYOL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYU-F01  JYU-F02
                                                JYU-F03  JYU-F04
                                                JYU-F06  JYU-F07
                                 STATUS         JYU-STATUS.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜ジョイ受領データ＞*****
*    FILE = ｹﾝｼｭｳﾃﾞｰﾀﾌｱｲﾙ
 FD  JOYJYR
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  IN-REC.
     03  JOY-REC            OCCURS    4.
        05  DEN-01                PIC  X(1).
        05  DEN-02                PIC  X(63).
*
*****<商品変換テーブル>*****
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*****<商品名称マスタ>*******
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*****<売上伝票ファイル>*****
 FD  SHTDENF            LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   SHT       PREFIX.
*****<店舗マスタ>***********
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*****<受領データ>***********
 FD  JYURYOF            LABEL RECORD   IS   STANDARD.
     COPY     JYURYOF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  JOY-STATUS               PIC  X(02).
     03  SHO-STATUS               PIC  X(02).
     03  MEI-STATUS               PIC  X(02).
     03  SHT-STATUS               PIC  X(02).
     03  TEN-STATUS               PIC  X(02).
     03  JYU-STATUS               PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
     03  CNT-GYO                  PIC  9(02)     VALUE   ZERO.
     03  READ-CNT                 PIC  9(07)     VALUE   ZERO.
     03  TAIS-CNT                 PIC  9(07)     VALUE   ZERO.
     03  WRITE-CNT                PIC  9(07)     VALUE   ZERO.
***  日付エリア
 01  WK-SYS-DATE.
     03  WK-YY1                   PIC  9(02)     VALUE   ZERO.
     03  WK-YMD.
         05  WK-YY2               PIC  9(02)     VALUE   ZERO.
         05  WK-MM                PIC  9(02)     VALUE   ZERO.
         05  WK-DD                PIC  9(02)     VALUE   ZERO.
 01  WK-DATE                      PIC  9(08)     VALUE   ZERO.
 01  CNT-PAGE                     PIC  9(04)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  PAGE-SW                  PIC  9(01)     VALUE   ZERO.
 01  HSHOTBL-INV-FLG              PIC  X(03)     VALUE   SPACE.
 01  HMEIMS-INV-FLG               PIC  X(03)     VALUE   SPACE.
 01  SHTDENF-INV-FLG              PIC  X(03)     VALUE   SPACE.
 01  HTENMS-INV-FLG               PIC  X(03)     VALUE   SPACE.
 01  JYURYOF-INV-FLG              PIC  X(03)     VALUE   SPACE.
 01  IX                           PIC  9(01)     VALUE   1.
 01  WK-SHT-F50                   PIC S9(09)V9   VALUE   ZERO.
*
 01  FILE-ERR010                  PIC  N(11)     VALUE
         NC"ジョイ受領ＤＴ異常！！".
 01  FILE-ERR020                  PIC  N(11)     VALUE
         NC"商品変換ＴＢＬ異常！！".
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"商品名称Ｍ　　異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"売上伝票Ｆ　　異常！！".
 01  FILE-ERR050                  PIC  N(11)     VALUE
         NC"店舗Ｍ　　　　異常！！".
 01  FILE-ERR060                  PIC  N(11)     VALUE
         NC"受領ＤＴ　　　異常！！".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKY6000B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*ヘッドレコード退避ワーク
 01  HD-REC.
     03  HD-01              PIC  X(01).
     03  HD-02              PIC  9(05).
     03  HD-03              PIC  9(04).
     03  HD-04              PIC  9(02).
     03  HD-05              PIC  9(09).
     03  HD-06              PIC  9(06).
     03  HD-07              PIC  9(08).
     03  HD-08              PIC  9(06).
     03  HD-09              PIC  9(10).
     03  HD-10              PIC  9(10).
     03  HD-11              PIC  X(01).
     03  HD-12              PIC  X(02).
*
*    明細レコード退避ワーク
 01  OUT-REC.
     03  OUT-01             PIC  X(01).
     03  OUT-02             PIC  9(02).
     03  OUT-03             PIC  X(13).
     03  OUT-04             PIC  9(07)V99.
     03  OUT-05             PIC  9(07).
     03  OUT-06             PIC  9(05)V9.
     03  OUT-07             PIC  9(10).
     03  OUT-08             PIC  9(10).
     03  OUT-09             PIC  X(01).
     03  OUT-10             PIC  X(05).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN            PIC X(01).
 01  LINK-IN-YMD6           PIC 9(06).
 01  LINK-IN-YMD8           PIC 9(08).
 01  LINK-OUT-RET           PIC X(01).
 01  LINK-OUT-YMD           PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TORICD.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << 受信ﾃﾞｰﾀ ｴﾗｰ >> ---*
 000-JOY-ERR            SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    JOYJYR.
     MOVE     "JOYJYR"         TO   ERR-FL-ID.
     MOVE      JOY-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 商品変換ﾃｰﾌﾞﾙ >> ---*
 000-HSHOTBL-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    HSHOTBL.
     MOVE     "HSHOTBL"        TO   ERR-FL-ID.
     MOVE      SHO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR020   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 商品名称ﾏｽﾀ >> ---*
 000-HMEIMS-ERR         SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    HMEIMS.
     MOVE     "HMEIMS"         TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR030   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 売上伝票ﾌｧｲﾙ >> ---*
 000-SHTDENF-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    SHTDENF.
     MOVE     "SHTDENF"        TO   ERR-FL-ID.
     MOVE      SHT-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR040   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 店舗ﾏｽﾀ >> ---*
 000-HTENMS-ERR         SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    HTENMS.
     MOVE     "HTENMS"         TO   ERR-FL-ID.
     MOVE      TEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR050   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 受領ﾃﾞｰﾀ >> ---*
 000-JYURYOF-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    JYURYOF.
     MOVE     "JYURYOF"        TO   ERR-FL-ID.
     MOVE      JYU-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR060   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL     END-FLG  =  "END".
     PERFORM            END-SEC.
*
 CONTROL-END.
     STOP     RUN.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     JOYJYR   HSHOTBL   HMEIMS   SHTDENF
                        HTENMS.
     OPEN     OUTPUT    JYURYOF.
*    ワーク初期化
     MOVE     SPACE              TO   END-FLG.
*    システム日付取得
     ACCEPT   WK-YMD    FROM     DATE.
     MOVE     "3"                TO   LINK-IN-KBN.
     MOVE     WK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO               TO   LINK-IN-YMD8.
     MOVE     ZERO               TO   LINK-OUT-RET.
     MOVE     ZERO               TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"      USING   LINK-IN-KBN
                                      LINK-IN-YMD6
                                      LINK-IN-YMD8
                                      LINK-OUT-RET
                                      LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD       TO   WK-DATE.
*    受信ファイル読込み
     PERFORM  FL-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*
     PERFORM  VARYING   IX  FROM  1  BY  1  UNTIL  ( IX > 4 )
              OR        ( DEN-01(IX)   =   SPACE )
       EVALUATE      DEN-01(IX)
              WHEN      "L"
                        MOVE      JOY-REC(IX)    TO   HD-REC
              WHEN      "N"
                        MOVE      JOY-REC(IX)    TO   OUT-REC
                        PERFORM   JYURYO-HEN-SEC
       END-EVALUATE
     END-PERFORM.
*    受信データ読込み
     PERFORM       FL-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             受領ＤＴ編集　処理                      2.5.1    *
****************************************************************
 JYURYO-HEN-SEC         SECTION.
*
      MOVE      SPACE          TO   JYU-REC.
      INITIALIZE                    JYU-REC.
***** 取引先コード
      MOVE      PARA-TORICD    TO   JYU-F01.
***** 発注日
*     MOVE      "3"            TO   LINK-IN-KBN
*     MOVE      WK-DEPB04      TO   LINK-IN-YMD6
*     MOVE      ZERO           TO   LINK-IN-YMD8
*     MOVE      ZERO           TO   LINK-OUT-RET
*     MOVE      ZERO           TO   LINK-OUT-YMD
*     CALL      "SKYDTCKB"  USING   LINK-IN-KBN
*                                   LINK-IN-YMD6
*                                   LINK-IN-YMD8
*                                   LINK-OUT-RET
*                                   LINK-OUT-YMD
*     MOVE      LINK-OUT-YMD   TO   JYU-F02
***** 納品日
      MOVE      HD-07          TO   JYU-F03.
***** 店舗ＣＤ
      MOVE      HD-02          TO   JYU-F04.
***** 店舗名
      PERFORM   TENMS-RD-SEC.
      MOVE      TEN-F04        TO   JYU-F05.
***** 伝票番号
      MOVE      HD-05          TO   JYU-F06.
***** 行番号
      MOVE      OUT-02         TO   JYU-F07.
***** 伝票区分
***** MOVE      40             TO   JYU-F08.
      MOVE      HD-04          TO   JYU-F08.
***** 商品ＣＤ
*     MOVE      WK-DEPD031     TO   JYU-F09.
***** 商品名１、２
      PERFORM   HSHOTBL-RD-SEC.
      IF  HSHOTBL-INV-FLG = SPACE
          PERFORM   HMEIMS-RD-SEC
          MOVE      MEI-F031       TO   JYU-F101
          MOVE      MEI-F032       TO   JYU-F102
      ELSE
          MOVE      ALL "#"        TO   JYU-F101
          MOVE      ALL "#"        TO   JYU-F102
          DISPLAY "SHO-F01 = " SHO-F01  UPON  CONS
          DISPLAY "SHO-F02 = " SHO-F02  UPON  CONS
      END-IF.
***** 発注数
      PERFORM   SHTDENF-RD-SEC.
      MOVE      WK-SHT-F50     TO   JYU-F11.
***** 検収数
      MOVE      OUT-06         TO   JYU-F12.
***** 原価単価
      MOVE      OUT-04         TO   JYU-F13.
***** 原価金額
      MOVE      OUT-07         TO   JYU-F14.
***** ＪＡＮＣＤ
      MOVE      OUT-03         TO   JYU-F15.
***** 予備１　　
      MOVE      HD-11          TO   JYU-F16.
***** 予備２
      MOVE      OUT-09         TO   JYU-F17.
***** 予備３　　
      MOVE      SPACE          TO   JYU-F18.
***** 予備４　　
      MOVE      SPACE          TO   JYU-F19.
***** 予備５
      MOVE      SPACE          TO   JYU-F20.
***** レコード出力
      WRITE     JYU-REC.
      ADD       1              TO   WRITE-CNT.
*
 JYURYO-HEN-EXIT.
     EXIT.
****************************************************************
*             ジョイ受領データ　ＲＥＡＤ処理             2.5.1 *
****************************************************************
 FL-READ-SEC            SECTION.
*ジョイ受領データ読込み
     READ     JOYJYR    AT        END
              MOVE     "END"      TO   END-FLG
              GO                  TO   FL-READ-EXIT
              NOT  AT  END
              ADD       1         TO   READ-CNT
     END-READ.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             商品変換ＴＢＬ　ＲＥＡＤ処理               2.5.1 *
****************************************************************
 HSHOTBL-RD-SEC       SECTION.
*
     MOVE     PARA-TORICD       TO   SHO-F01.
     MOVE     OUT-03            TO   SHO-F02.
*商品変換テーブル読込
     READ     HSHOTBL
              INVALID
                  MOVE  "INV"   TO   HSHOTBL-INV-FLG
              NOT INVALID
                  MOVE  SPACE   TO   HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-RD-EXIT.
     EXIT.
****************************************************************
*             商品名称Ｍ　ＲＥＡＤ処理                  2.5.1  *
****************************************************************
 HMEIMS-RD-SEC       SECTION.
*
     MOVE     SHO-F031          TO   MEI-F01.
     MOVE     SHO-F0321         TO   MEI-F0121.
     MOVE     SHO-F0322         TO   MEI-F0122.
     MOVE     SHO-F0323         TO   MEI-F0123.
*商品マスタ読込
     READ     HMEIMS
              INVALID
                  MOVE  "INV"       TO   HMEIMS-INV-FLG
                  MOVE  ALL "*"     TO   MEI-F031
                  MOVE  ALL "*"     TO   MEI-F032
              NOT INVALID
                  MOVE  SPACE   TO   HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-RD-EXIT.
     EXIT.
****************************************************************
*             売上伝票Ｆ　ＲＥＡＤ処理                  2.5.1  *
****************************************************************
 SHTDENF-RD-SEC       SECTION.
*
     MOVE     PARA-TORICD       TO   SHT-F01.
     MOVE     HD-05             TO   SHT-F02.
     MOVE     OUT-02            TO   SHT-F03.
     MOVE     ZERO              TO   SHT-F04.
     MOVE     40                TO   SHT-F051.
***2011.11.28 ST
     MOVE     HD-02             TO   SHT-F07.
     MOVE     HD-07             TO   SHT-F112.
***2011.11.28 EN
*****DISPLAY "SHT-F01  = " SHT-F01  UPON CONS.
*****DISPLAY "SHT-F02  = " SHT-F02  UPON CONS.
*****DISPLAY "SHT-F03  = " SHT-F03  UPON CONS.
*****DISPLAY "SHT-F04  = " SHT-F04  UPON CONS.
*****DISPLAY "SHT-F051 = " SHT-F051 UPON CONS.
*売上伝票ファイル読込
     READ     SHTDENF
              INVALID
                  MOVE  "INV"   TO   SHTDENF-INV-FLG
                  MOVE  ZERO    TO   WK-SHT-F50
                  DISPLAY "INV" UPON CONS
              NOT INVALID
                  MOVE  SPACE   TO   SHTDENF-INV-FLG
                  MOVE  SHT-F15 TO   WK-SHT-F50
*****DISPLAY "O K = " WK-SHT-F50 " : " SHT-F15 UPON CONS
     END-READ.
*
 SHTDENF-RD-EXIT.
     EXIT.
****************************************************************
*             店舗Ｍ　ＲＥＡＤ処理                  2.5.1      *
****************************************************************
 TENMS-RD-SEC       SECTION.
*
     MOVE     PARA-TORICD       TO   TEN-F52.
     MOVE     HD-02             TO   TEN-F011.
*店舗マスタ読込
     READ     HTENMS
              INVALID
                  MOVE  "INV"   TO   HTENMS-INV-FLG
              NOT INVALID
                  MOVE  SPACE   TO   HTENMS-INV-FLG
     END-READ.
*
 TENMS-RD-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    JOYJYR   HSHOTBL   HMEIMS   SHTDENF
              HTENMS   JYURYOF.
*
     DISPLAY "READ-CNT  = " READ-CNT  UPON CONS.
*    DISPLAY "TAIS-CNT  = " TAIS-CNT  UPON CONS.
     DISPLAY "WRITE-CNT = " WRITE-CNT UPON CONS.
*
 END-EXIT.
     EXIT.

```
