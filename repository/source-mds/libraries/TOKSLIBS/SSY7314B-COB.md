# SSY7314B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7314B.COB`

## ソースコード

```cobol
****************************************************************
*        SYSTEM ･･･ダイユーエイト物品受領書データ作成          *
*        PG-NAME･･･物品受領書データ作成                        *
*        PG-ID  ･･･SSY7314B                                    *
*                  CREATE-DATE:10.07.02  BY.NAV                *
*                  CHANGE-DATE:13.09.19  BY.NAV                *
*                             :日敷追加                        *
*                  CHANGE-DATE:18.01.29  BY.NAV TAKAHASHI      *
*                             :リック資材／植物追加            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY7314B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/07/02.
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
*    受信受領データＦ
     SELECT   DYJFILE            ASSIGN    TO   DYJFILE
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   DYJ-STATUS.
*    受領書ファイル（ダイユーエイト）
     SELECT   DYJRYOF1           ASSIGN    TO   DYJRYOF1
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   JY1-STATUS.
*    受領書ファイル（エイトファーム)
     SELECT   DYJRYOF2           ASSIGN    TO   DYJRYOF2
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   JY2-STATUS.
*2013/09/19 日敷追加 ↓
*    受領書ファイル（日敷)
     SELECT   DYJRYOF3           ASSIGN    TO   DYJRYOF3
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   JY3-STATUS.
*2013/09/19 日敷追加 ↑
*#2018/01/29 NAV ST ↓
*****受領書ファイル（リック資材)
     SELECT   DYJRYOF4           ASSIGN    TO   DYJRYOF4
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   JY4-STATUS.
*****受領書ファイル（リック植物)
     SELECT   DYJRYOF5           ASSIGN    TO   DYJRYOF5
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   JY5-STATUS.
*#2018/01/29 NAV ED ↑
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜受信受領データＦ＞*****
*    FILE = ｼﾞｭｼﾝｼﾞｭﾘｮｳｼｮﾃﾞｰﾀ
 FD  DYJFILE
                        BLOCK CONTAINS      7    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  DYJ-REC.
     03  DYJ-01                   PIC  X(02).
     03  DYJ-02                   PIC  X(576).
*
*****<受領ファイル（ダイユーエイト）>*****
 FD  DYJRYOF1
                        BLOCK CONTAINS      7    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  JY1-REC.
     03  FILLER                   PIC  X(197).
*
*****<受領ファイル（エイトファーム）>*****
 FD  DYJRYOF2
                        BLOCK CONTAINS      7    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  JY2-REC.
     03  FILLER                   PIC  X(197).
*
*2013/09/19 日敷追加 ↓
*****<受領ファイル（日敷）>*****
 FD  DYJRYOF3
                        BLOCK CONTAINS      7    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  JY3-REC.
     03  FILLER                   PIC  X(197).
*2013/09/19 日敷追加 ↑
*#2018/01/29 NAV ST リック　↓
*****<受領ファイル（リック資材）>*****
 FD  DYJRYOF4
                        BLOCK CONTAINS      7    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  JY4-REC.
     03  FILLER                   PIC  X(197).
*****<受領ファイル（リック植物）>*****
 FD  DYJRYOF5
                        BLOCK CONTAINS      7    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  JY5-REC.
     03  FILLER                   PIC  X(197).
*#2018/01/29 NAV ED リック　↑
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*受領データ（ヘッダ）
     COPY     DYJURHDF  OF   XFDLIB   JOINING   JHD    PREFIX.
*受領データ（明細）
     COPY     DYJURMEF  OF   XFDLIB   JOINING   JME    PREFIX.
*
*編集用　受領データ
     COPY     DYJRYOF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  DYJ-STATUS               PIC  X(02).
     03  JY1-STATUS               PIC  X(02).
     03  JY2-STATUS               PIC  X(02).
*2013/09/19 日敷追加 ↓
     03  JY3-STATUS               PIC  X(02).
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
     03  JY4-STATUS               PIC  X(02).
     03  JY5-STATUS               PIC  X(02).
*#2018/01/29 リック追加 ↑
*
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  IX                       PIC  9(02)     VALUE   ZERO.
     03  IY                       PIC  9(02)     VALUE   ZERO.
     03  CNT-GYO                  PIC  9(02)     VALUE   ZERO.
     03  READ-CNT                 PIC  9(07)     VALUE   ZERO.
     03  TAIS-CNT                 PIC  9(07)     VALUE   ZERO.
     03  WRITE-CNT1               PIC  9(07)     VALUE   ZERO.
     03  WRITE-CNT2               PIC  9(07)     VALUE   ZERO.
*2013/09/19 日敷追加 ↓
     03  WRITE-CNT3               PIC  9(07)     VALUE   ZERO.
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
     03  WRITE-CNT4               PIC  9(07)     VALUE   ZERO.
     03  WRITE-CNT5               PIC  9(07)     VALUE   ZERO.
*#2018/01/29 リック追加 ↑
*
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
 01  DYJYROF-INV-FLG              PIC  X(03)     VALUE   SPACE.
*
 01  WK-HATYUSYA.
     03  WK-HATYUSYA1             PIC  X(04).
     03  FILLER                   PIC  X(09).
*
 01  WK-HENKAN-10.
     03  WK-HEN-10                PIC  X(10).
     03  WK-HEN-10-R              REDEFINES WK-HEN-10.
         05  WK-HEN-10-H          PIC  9(10).
     03  WK-HEN-10-FLG            PIC  X(01)     VALUE   SPACE.
     03  WK-HEN-10-OK             PIC S9(07)V9(02) VALUE   ZERO.
*
 01  WK-HENKAN-4.
     03  WK-HEN-4                 PIC  X(05)     VALUE   ZERO.
     03  WK-HEN-4-R               REDEFINES WK-HEN-4.
         05  WK-HEN-4-H           PIC  9(05).
     03  WK-HEN-4-FLG             PIC  X(01)     VALUE   SPACE.
     03  WK-HEN-4-OK              PIC S9(04)     VALUE   ZERO.
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"受信Ｆ　　　異常！！".
 01  FILE-ERR020                  PIC  N(12)     VALUE
         NC"受領ＤＴ　Ｆ１　異常！！".
 01  FILE-ERR021                  PIC  N(12)     VALUE
         NC"受領ＤＴ　Ｆ２　異常！！".
*2013/09/19 日敷追加 ↓
 01  FILE-ERR022                  PIC  N(12)     VALUE
         NC"受領ＤＴ　Ｆ３　異常！！".
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
 01  FILE-ERR023                  PIC  N(12)     VALUE
         NC"受領ＤＴ　Ｆ４　異常！！".
 01  FILE-ERR024                  PIC  N(12)     VALUE
         NC"受領ＤＴ　Ｆ５　異常！！".
*#2018/01/29 リック追加 ↑
*
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY7314B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN            PIC X(01).
 01  LINK-IN-YMD6           PIC 9(06).
 01  LINK-IN-YMD8           PIC 9(08).
 01  LINK-OUT-RET           PIC X(01).
 01  LINK-OUT-YMD           PIC 9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << 受信ﾃﾞｰﾀ ｴﾗｰ >> ---*
 000-DYJFILE-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DYJFILE.
     MOVE     "DYJFILE"        TO   ERR-FL-ID.
     MOVE      DYJ-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 受領ﾃﾞｰﾀ （ダイユーエイト）>> ---*
 000-DYJRYOF1-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DYJRYOF1.
     MOVE     "DYJRYOF1"       TO   ERR-FL-ID.
     MOVE      JY1-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR020   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 物品受領ﾃﾞｰﾀ（エイトファーム） >> ---*
 000-DYJYROF2-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DYJRYOF2.
     MOVE     "DYJRYOF2"       TO   ERR-FL-ID.
     MOVE      JY1-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR021   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*2013/09/19 日敷追加 ↓
*--- << 物品受領ﾃﾞｰﾀ（日敷） >> ---*
 000-DYJYROF3-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DYJRYOF3.
     MOVE     "DYJRYOF3"       TO   ERR-FL-ID.
     MOVE      JY3-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR022   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
*--- << 物品受領ﾃﾞｰﾀ（リック資材） >> ---*
 000-DYJYROF4-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DYJRYOF4.
     MOVE     "DYJRYOF4"       TO   ERR-FL-ID.
     MOVE      JY4-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR023   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 000-DYJYROF5-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DYJRYOF5.
     MOVE     "DYJRYOF5"       TO   ERR-FL-ID.
     MOVE      JY5-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR024   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*2013/09/19 日敷追加 ↑
*
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
     OPEN     INPUT     DYJFILE.
     OPEN     OUTPUT    DYJRYOF1.
     OPEN     OUTPUT    DYJRYOF2.
*2013/09/19 日敷追加 ↓
     OPEN     OUTPUT    DYJRYOF3.
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
     OPEN     OUTPUT    DYJRYOF4  DYJRYOF5.
*#2018/01/29 リック追加 ↑
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
*    ヘッダ情報退避
     IF       DYJ-01    =   "HD"
              MOVE      DYJ-REC        TO   JHD-REC
              GO        TO   MAIN-000
     END-IF.
     IF       DYJ-01    =   "DT"
              MOVE      DYJ-REC        TO   JME-REC
              PERFORM HENSYU-SEC
     END-IF.
*
 MAIN-000.
*    受信データ読込み
     PERFORM       FL-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     READ     DYJFILE  AT        END
              MOVE     "END"      TO   END-FLG
              GO                  TO   FL-READ-EXIT
              NOT  AT  END
              ADD       1         TO   READ-CNT
     END-READ.
*
     IF       DYJ-01  =  "HD" OR "DT"
              ADD       1         TO   TAIS-CNT
     ELSE
              GO                  TO   READ-000
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             受領データ出力                        2.5.1      *
****************************************************************
 HENSYU-SEC             SECTION.
*    レコードクリア
*2013/09/19 日敷追加 ↓
***  MOVE      SPACE     TO   JY1-REC  JY2-REC  JYU-REC.
***  INITIALIZE               JY1-REC  JY2-REC  JYU-REC.
     MOVE      SPACE     TO   JY1-REC  JY2-REC  JY3-REC  JYU-REC.
     INITIALIZE               JY1-REC  JY2-REC  JY3-REC  JYU-REC.
*2013/09/19 日敷追加 ↑
*#2018/01/29 NAV ST リック追加
     MOVE      SPACE     TO   JY4-REC  JY5-REC.
     INITIALIZE               JY4-REC  JY5-REC.
*#2018/01/29 NAV ED リック追加
*
*****DISPLAY "JHD-H12 = " JHD-H12 UPON CONS.
     MOVE   JHD-H12           TO   WK-HATYUSYA.
*
*    取引先コード
     IF     WK-HATYUSYA1  =  "4950"
************MOVE    JHD-H04   TO   JYU-F00
            MOVE    1225      TO   JYU-F00
     END-IF.
*2010/11/25 ﾀﾞｲﾕｰｴｲﾄMAXの場合、ﾀﾞｲﾕｰｴｲﾄに含める
     IF     WK-HATYUSYA1  =  "4959"
************MOVE    JHD-H04   TO   JYU-F00
            MOVE    1225      TO   JYU-F00
     END-IF.
     IF     WK-HATYUSYA1  =  "4952"
            MOVE   12251      TO   JYU-F00
     END-IF.
*2013/09/19 日敷追加 ↓
     IF     WK-HATYUSYA1  =  "4953"
            MOVE   12252      TO   JYU-F00
     END-IF.
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
     IF     WK-HATYUSYA1  =  "4978"
************DISPLAY "AAA" UPON CONS
            MOVE JHD-H04(1:6) TO   JYU-F00
     END-IF.
*#2018/01/29 リック追加 ↑
*    受信日
     MOVE      WK-DATE        TO   JYU-F01.
*    発注日
     IF   JHD-H07 NOT  NUMERIC
          MOVE      ZERO      TO   JYU-F02
     ELSE
          MOVE      JHD-H07   TO   JYU-F02
     END-IF.
*    納品日
*****MOVE      JHD-H08        TO   JYU-F03.
     IF   JHD-H08 NOT  NUMERIC
          MOVE      ZERO      TO   JYU-F03
     ELSE
          MOVE      JHD-H08   TO   JYU-F03
     END-IF.
*    店舗ＣＤ
     MOVE      JHD-H17        TO   JYU-F04.
*    店舗名
     MOVE      JHD-H15        TO   JYU-F05.
*    伝票番号
     MOVE      JHD-H03        TO   JYU-F06.
*    行番号
     MOVE      JME-M02        TO   JYU-F07.
*    伝票区分
     MOVE      JHD-H10        TO   JYU-F08.
*    商品ＣＤ
     MOVE      JME-M04        TO   JYU-F09.
*    商品名１、２
     MOVE      JME-M06        TO   JYU-F10.
*    発注数
     COMPUTE   JYU-F11  =   JME-M12  /  10.
*    検収数
     IF  JHD-H24(1:1) = "0"
         MOVE  JME-M08        TO   JYU-F12
         COMPUTE   JYU-F12  =   JME-M08  /  10
     ELSE
         COMPUTE  JYU-F12  =  ( JME-M08  *  -1 ) / 10
     END-IF.
*    原価単価
     COMPUTE   JYU-F13  =   JME-M10  /  100.
*    原価金額
     IF  JHD-H24 = 0
         MOVE  JME-M09        TO   JYU-F14
     ELSE
         COMPUTE  JYU-F14  =  JME-M09  *  -1
     END-IF.
*    ＪＡＮＣＤ
     MOVE      JME-M04        TO   JYU-F15.
*    納品書番号
     MOVE      SPACE          TO   JYU-F16.
*    レコード出力
     IF     WK-HATYUSYA1  =  "4950"
            MOVE   JYU-REC    TO   JY1-REC
            WRITE  JY1-REC
            ADD    1          TO   WRITE-CNT1
     END-IF.
*2010/11/25 ﾀﾞｲﾕｰｴｲﾄMAXの場合、ﾀﾞｲﾕｰｴｲﾄに含める
     IF     WK-HATYUSYA1  =  "4959"
            MOVE   JYU-REC    TO   JY1-REC
            WRITE  JY1-REC
            ADD    1          TO   WRITE-CNT1
     END-IF.
     IF     WK-HATYUSYA1  =  "4952"
            MOVE   JYU-REC    TO   JY2-REC
            WRITE     JY2-REC
            ADD    1          TO   WRITE-CNT2
     END-IF.
*2013/09/19 日敷追加 ↓
     IF     WK-HATYUSYA1  =  "4953"
            MOVE   JYU-REC    TO   JY3-REC
            WRITE     JY3-REC
            ADD    1          TO   WRITE-CNT3
     END-IF.
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
     IF     WK-HATYUSYA1  =  "4978"
************DISPLAY "JYU-F00 = " JYU-F00 UPON CONS
            IF  JYU-F00   =  1994
                MOVE   JYU-REC    TO   JY4-REC
                WRITE     JY4-REC
                ADD    1          TO   WRITE-CNT4
            END-IF
            IF  JYU-F00   =  1995
                MOVE   JYU-REC    TO   JY5-REC
                WRITE     JY5-REC
                ADD    1          TO   WRITE-CNT5
            END-IF
     END-IF.
*#2018/01/29 リック追加 ↑
*
 HENSYU-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    DYJFILE  DYJRYOF1 DYJRYOF2.
*2013/09/19 日敷追加 ↓
     CLOSE    DYJRYOF3.
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
     CLOSE    DYJRYOF4  DYJRYOF5.
*#2018/01/29 リック追加 ↑
*
     DISPLAY "READ-CNT   = " READ-CNT   UPON CONS.
     DISPLAY "TAIS-CNT   = " TAIS-CNT   UPON CONS.
     DISPLAY "WRITE-CNT1 = " WRITE-CNT1 UPON CONS.
     DISPLAY "WRITE-CNT2 = " WRITE-CNT2 UPON CONS.
*2013/09/19 日敷追加 ↓
     DISPLAY "WRITE-CNT3 = " WRITE-CNT3 UPON CONS.
*2013/09/19 日敷追加 ↑
*#2018/01/29 リック追加 ↓
     DISPLAY "WRITE-CNT4 = " WRITE-CNT4 UPON CONS.
     DISPLAY "WRITE-CNT5 = " WRITE-CNT5 UPON CONS.
*#2018/01/29 リック追加 ↑
*
 END-EXIT.
     EXIT.

```
