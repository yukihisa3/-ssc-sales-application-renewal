# ZHA0040O

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZHA0040O.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　発注入力　　　　　　　　　　　　　*
*    作成日／更新日　　　：　93/05/06                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注ファイルの登録・修正・削除　　*
*                            照会を行う　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZHA0040O.
 AUTHOR.                APL.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        DESTINATION-1       IS   DSP-WSNO
                        FILE      STATUS    IS   DSP-STATUS.
*
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*
*---<<  店舗マスタ  >>---*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*
*---<<  仕入先マスタ  >>---*
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHI-F01
                        FILE      STATUS    IS   SHI-STATUS.
*
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F031
                                                 SHO-F032
                        FILE      STATUS    IS   SHO-STATUS.
*
*---<<  商品在庫マスタ  >>---*
     SELECT   ZZAIMS    ASSIGN    TO        DA-01-VI-ZZAIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*
*---<<  発注ファイル  >>---*
     SELECT   ZHACHDT   ASSIGN    TO        DA-01-VI-ZHACHDT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   HAC-F01
                                                 HAC-F02
                                                 HAC-F03
                                                 HAC-F04
                                                 HAC-F05
                        FILE      STATUS    IS   HAC-STATUS.
*
*---<<  入庫ファイル  >>---*
     SELECT   ZNYUKDT   ASSIGN    TO        DA-01-VI-ZNYUKDT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   NYK-F01
                                                 NYK-F02
                                                 NYK-F03
                                                 NYK-F04
                                                 NYK-F05
                        FILE      STATUS    IS   NYK-STATUS.
*
*---<<  本日分発注ファイル  >>---*
     SELECT   ZHACHWK   ASSIGN    TO        DA-01-S-ZHACHWK
                        FILE      STATUS    IS   HWK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(2000).
     COPY     ZHA0040   OF        XMDLIB.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  仕入先マスタ  >>---*
 FD  ZSHIMS.
     COPY     ZSHIMS    OF        XFDLIB
              JOINING   SHI       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  商品コード変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZZAIMS.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  発注ファイル  >>---*
 FD  ZHACHDT.
     COPY     ZHACHDT.
*---<<  入庫ファイル  >>---*
 FD  ZNYUKDT.
     COPY     ZNYUKDT   OF        XFDLIB
              JOINING   NYK       PREFIX.
*---<<  本日分発注ファイル  >>---*
 FD  ZHACHWK.
 01  HWK-REC                 PIC  X(200).
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
     03  DSP-WSNO            PIC  X(08).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JYO-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
     02  SHI-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  SHO-STATUS          PIC  X(02).
     02  ZAI-STATUS          PIC  X(02).
     02  HAC-STATUS          PIC  X(02).
     02  NYK-STATUS          PIC  X(02).
     02  HWK-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  EOF-FLG             PIC  X(01)  VALUE SPACE.
     02  READ-FLG            PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
 01  UPDT-FLG-AREA.
     02  UPDT-FLG            PIC  X(01)  OCCURS 6.
     02  UPDT-FLG00          PIC  X(01).
     02  UPDT-FLG80          PIC  X(01).
     02  UPDT-FLG99          PIC  X(01).
****  処理スイッチ  ****
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01).
****  インデックス  ****
     02  IXA                 PIC  9(02).
     02  IXB                 PIC  9(02).
     02  IXC                 PIC  9(02).
     02  IXD                 PIC  9(02).
****  システム日付  ****
 01  SYS-DATE                PIC  9(06).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE2                PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
****  テーブル領域  ****
 01  TBL-AREA.
     03  TBL-DATA            OCCURS 6.
         05  TB-FLG          PIC  9(01).         *>発注済フラグ
 01  DLT-TABLE.
     03  DLT-DATA            OCCURS 6.
         05  DLT-SHOCD       PIC  X(08).
         05  DLT-HINTN1      PIC  X(05).
         05  DLT-HINTN2      PIC  X(02).
         05  DLT-HINTN3      PIC  X(01).
         05  DLT-SURYO       PIC S9(09)V99.
         05  DLT-F36         PIC  9(01).
*\\\ 93.06.01 START \\\
 01  TORICD-FLG              PIC  9(01)          VALUE  ZERO.
 01  SAV-AREA.
     03  SAV-DATA            OCCURS 6.
         05  SAV-SHOCD       PIC  X(08).
         05  SAV-HINTN1      PIC  X(05).
         05  SAV-HINTN2      PIC  X(02).
         05  SAV-HINTN3      PIC  X(01).
         05  SAV-SURYO       PIC S9(09)V99.
         05  SAV-TORICD      PIC  9(08).
         05  SAV-SOKO        PIC  X(02).
         05  SAV-TANA        PIC  X(06).
*\\\ 93.06.01 END   \\\
 01  OLD-SURYO-AREA.
     03  OLD-SURYO           PIC S9(09)V99       OCCURS 6.
     03  OLD-F36             PIC  9(01)          OCCURS 6.
 01  DAY-TABLE               PIC  X(24)      VALUE
                                  "312831303130313130313031".
 01  DAY-TABLE-RD            REDEFINES       DAY-TABLE.
     03  DAY-T               PIC  9(02)      OCCURS 12.
****  チェック用    ****
 01  CHK-YMD                 PIC  9(08).
 01  CHK-YMD-RD              REDEFINES           CHK-YMD.
     03  CHK-YY              PIC  9(04).
     03  CHK-MM              PIC  9(02).
     03  CHK-DD              PIC  9(02).
 01  CHK-SOK                 PIC  9(02).
 01  CHK-SURYO1              PIC S9(09)V99.
 01  CHK-SURYO2              PIC S9(09)V99.
 01  CHK-SHO                 PIC  9(09).
 01  CHK-R4                  PIC  9(09).
 01  CHK-R100                PIC  9(09).
 01  CHK-R400                PIC  9(09).
****  作業用    ****
 01  SEIREKI                 PIC  9(08).
 01  WK-MSG                  PIC  N(36).
 01  WK-MSG-RD               REDEFINES           WK-MSG.
     03  WK-MSG1             PIC  N(17).
     03  WK-MSG2             PIC  N(17).
 01  WK-SURYO                PIC S9(09)V99.
 01  WK-KIN                  PIC S9(09).
 01  WK-TANA                 PIC  X(06).
 01  SV-DENKU                PIC  9(02)         VALUE ZERO.
 01  SV-HACNO1               PIC  9(07)         VALUE ZERO.
 01  SV-HACNO2               PIC  9(07)         VALUE ZERO.
 01  SV-DSPREC               PIC  X(2000).
 01  WK-LSIME                PIC  9(06).
 01  WK-LSIMER               REDEFINES  WK-LSIME.
     03  WK-LSIMER1          PIC  9(04).
     03  WK-LSIMER2          PIC  9(02).
 01  WK-NOUHIN               PIC  9(08)    VALUE ZERO.
 01  WK-NOUHIN-R  REDEFINES  WK-NOUHIN.
     03  WK-NOUHIN1          PIC  9(06).
     03  WK-NOUHIN2          PIC  9(02).
****  チェックデジット算出サブルーチン作業用    ****
 01  LINK-AREA.
     03  LI-KBN              PIC  9(01).
     03  LI-KETA             PIC  9(01).
     03  LI-START            PIC  9(09).
     03  LI-END              PIC  9(09).
     03  LI-DENNO            PIC  9(09).
     03  LO-ERR              PIC  9(01).
     03  LO-NEXT             PIC  9(09).
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01            PIC N(07) VALUE
             NC"_取消　_終了".
     02  PMSG02            PIC N(19) VALUE
             NC"_取消　_終了　_戻り　_前頁　_次頁".
     02  PMSG03            PIC N(11) VALUE
             NC"_取消　_終了　_戻り".
     02  PMSG04            PIC N(07) VALUE
             NC"_取消　_戻り".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZHA0040O".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(20)  VALUE
             NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"処理区分が違います".
     02  MSG-ERR3            PIC  N(20)  VALUE
             NC"入力項目に間違いがあります".
     02  MSG-ERR4            PIC  N(20) VALUE
            NC"Ｙ，Ｈ，Ｂのいずれかで入力して下さい".
     02  MSG-ERR5            PIC  N(20) VALUE
            NC"ＹかＨで入力して下さい".
     02  MSG-ERR6            PIC  N(20) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"該当データがありません".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"前レコードがありません".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"次レコードがありません".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"計上フラグに誤りがあります".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 10  TIMES.
*８桁変換後セット
 01  NEW-HACYMD            PIC 9(08)  VALUE  ZERO.
 01  NEW-NOUKI             PIC 9(08)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE     "HJYOKEN"        TO   ERR-FL-ID.
     MOVE      JYO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE     "HTOKMS "        TO   ERR-FL-ID.
     MOVE      TOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE     "HTENMS "        TO   ERR-FL-ID.
     MOVE      TEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSHIMS.
     MOVE     "ZSHIMS "        TO   ERR-FL-ID.
     MOVE      SHI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS.
     MOVE     "ZSOKMS "        TO   ERR-FL-ID.
     MOVE      SOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE     "HMEIMS "        TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE     "HSHOTBL"        TO   ERR-FL-ID.
     MOVE      SHO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZZAIMS.
     MOVE     "ZZAIMS "        TO   ERR-FL-ID.
     MOVE      ZAI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZHACHDT.
     MOVE     "ZHACHDT"        TO   ERR-FL-ID.
     MOVE      HAC-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZNYUKDT.
     MOVE     "ZNYUKDT"        TO   ERR-FL-ID.
     MOVE      NYK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZHACHWK.
     MOVE     "ZHACHWK"        TO   ERR-FL-ID.
     MOVE      NYK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
************************************************************
 ZHA0040O-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 ZHA0040O-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF      HJYOKEN   ZZAIMS    ZHACHDT
                        ZNYUKDT.
     OPEN     INPUT     HTOKMS    HTENMS    ZSHIMS    ZSOKMS
                        HMEIMS    HSHOTBL.
     OPEN     EXTEND    ZHACHWK.
*
*\\  93.07.09 \\
     MOVE     99             TO   JYO-F01.
     MOVE     "ZAI"          TO   JYO-F02.
     PERFORM  JYO-READ-SUB
     IF  INVALID-FLG   =     ZERO
         COMPUTE WK-LSIME    =    JYO-F05  +  1
         IF   WK-LSIMER2     >    12
              MOVE  1        TO   WK-LSIMER2
              ADD   1        TO   WK-LSIMER1
         END-IF
     END-IF.
*
*\\
*****ACCEPT   SYS-DATE       FROM DATE.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
     DISPLAY "SYS-DATE2 = " SYS-DATE2  UPON CONS.
*画面表示日付編集
     MOVE      SYS-DATE2(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE    "1"             TO   MAIN-FLG.
*
     MOVE     57             TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM         JYO-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         COMPUTE SEIREKI     =    JYO-F04  *  10000
         REWRITE                  JYO-REC
     ELSE
         MOVE    ZERO        TO   SEIREKI
     END-IF.
*
     MOVE     SPACE          TO   ZHA0040.
     PERFORM         DSP-WRITE-SUB.
     MOVE     65             TO   JYO-F01.
     MOVE     DSP-WSNO       TO   JYO-F02.
     PERFORM         JYO-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         MOVE     JYO-F04    TO   CHK-SOK
         REWRITE                  JYO-REC
     ELSE
         MOVE   ZERO         TO   CHK-SOK
     END-IF.
     PERFORM         EDIT-SET-SYORI.
     PERFORM         EDIT-SET-HEAD.
     PERFORM         EDIT-SET-BODY.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"  PERFORM   SYORI-SUB
         WHEN      "2"  PERFORM   HACNO-SUB
         WHEN      "3"  PERFORM   HEAD-SUB
         WHEN      "4"  PERFORM   BODY-SUB
         WHEN      "5"  PERFORM   KAKUNIN-SUB
         WHEN           OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*==========================================================*
*      2.1       処理区分入力              MAIN-FLG=1      *
*==========================================================*
 SYORI-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG01         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE    "SYORI"         TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE   SPACE   TO   ZHA0040
                        PERFORM             EDIT-SET-SYORI
                        PERFORM             EDIT-SET-HEAD
                        PERFORM             EDIT-SET-BODY
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "E000"
                        PERFORM   SYORICHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   SYORIK  TO   WK-SYORI
                            IF  SYORIK     =    1
                                PERFORM    HACNO-SET-SUB
                                PERFORM    HACYMD-SET-SUB
                                MOVE   "3"     TO   MAIN-FLG
                            ELSE
                                MOVE   "2"     TO   MAIN-FLG
                            END-IF
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 SYORI-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1     処理区分の入力チェック                    *
*----------------------------------------------------------*
 SYORICHK-SUB             SECTION.
     PERFORM         EDIT-SET-SYORI.
     MOVE     ZERO           TO   ERR-MSG-CD.
*処理区分 CHK
     IF  SYORIK    NOT  NUMERIC
         MOVE   ZERO         TO   SYORIK
     END-IF.
     EVALUATE   SYORIK
         WHEN   1
             MOVE   NC"登録"     TO   SYORIM
         WHEN   2
             MOVE   NC"修正"     TO   SYORIM
         WHEN   3
             MOVE   NC"削除"     TO   SYORIM
         WHEN   9
             MOVE   NC"照会"     TO   SYORIM
         WHEN   OTHER
             MOVE   02           TO   ERR-MSG-CD
             MOVE   "R"          TO   EDIT-OPTION  OF  SYORIK
             MOVE   "C"          TO   EDIT-CURSOR  OF  SYORIK
     END-EVALUATE.
     MOVE    ZERO           TO   SV-DENKU.
     MOVE    ZERO           TO   SV-HACNO1.
     MOVE    ZERO           TO   SV-HACNO2.
*
 SYORICHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.2     発注_自動採番                            *
*----------------------------------------------------------*
 HACNO-SET-SUB            SECTION.
     MOVE     60             TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM         JYO-READ-SUB.
     ADD      1              TO   JYO-F04.
     IF       JYO-F04  =   ZERO
         MOVE    1           TO   JYO-F04
     END-IF.
     MOVE     JYO-F04        TO   HACNO1.
     MOVE     ZERO           TO   HACNO2.
     IF  INVALID-FLG   =   ZERO
         REWRITE                  JYO-REC
     END-IF.
     INITIALIZE                   TBL-AREA.
 HACNO-SET-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.3     発注日セット                              *
*----------------------------------------------------------*
 HACYMD-SET-SUB           SECTION.
*## 1999/12/20 NAV
*****MOVE     SYS-DATE       TO   HACYMD.
     MOVE     WK-DATE        TO   HACYMD.
     MOVE     ZERO           TO   SEIKYU.
     MOVE     ZERO           TO   HAKKO.
     MOVE     ZERO           TO   ZEIKU.
 HACYMD-SET-END.
     EXIT.
*==========================================================*
*      2.2       伝区／発注_入力          MAIN-FLG=2      *
*==========================================================*
 HACNO-SUB           SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE    "HACNO"         TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                        MOVE    "1"    TO   MAIN-FLG
         WHEN   "F011"
                        PERFORM   HAC-BACK-SUB
         WHEN   "F012"
                        PERFORM   HAC-NEXT-SUB
         WHEN   "E000"
                        PERFORM   HACNOCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   HACDSP-SUB
                            IF   WK-SYORI  =    3  OR  9
                                 MOVE   "5"     TO   MAIN-FLG
                            ELSE
                                 MOVE   "3"     TO   MAIN-FLG
                            END-IF
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 HACNO-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     伝区／発注_の入力チェック                *
*----------------------------------------------------------*
 HACNOCHK-SUB            SECTION.
     PERFORM         EDIT-SET-HEAD.
     MOVE       ZERO    TO   ERR-MSG-CD.
****  伝区のチェック  ****
     IF  DENKU     NOT  NUMERIC
         MOVE   ZERO         TO   DENKU
     END-IF.
     MOVE     01             TO   JYO-F01.
     MOVE     DENKU          TO   JYO-F02.
     PERFORM         JYO-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         MOVE     JYO-F03    TO   DENKUM
         REWRITE                  JYO-REC
     ELSE
         MOVE   03      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DENKU
         MOVE   "C"     TO   EDIT-CURSOR  OF  DENKU
     END-IF.
****  発注_のチェック  ****
     IF  HACNO2    NOT  NUMERIC
         MOVE   ZERO         TO   HACNO2
     END-IF.
*
     IF  HACNO1         NUMERIC
         MOVE    DENKU          TO   SV-DENKU
         MOVE    HACNO1         TO   SV-HACNO1
         MOVE    HACNO2         TO   SV-HACNO2
*
         MOVE    "1"            TO   INVALID-FLG
         MOVE    SPACE          TO   READ-FLG
         PERFORM    HAC-START-SUB
         IF  EOF-FLG  =  ZERO
             PERFORM    HAC-READ-SUB
             PERFORM    HAC-CHK-SUB
             PERFORM  UNTIL (EOF-FLG      =  "1"     )  OR
                            (HAC-F01  NOT = SV-DENKU )  OR
                            (HAC-F02  NOT = SV-HACNO1)  OR
                            (HAC-F03  NOT = SV-HACNO2)
                 PERFORM    HAC-READ-SUB
                 MOVE   ZERO    TO   INVALID-FLG
             END-PERFORM
         END-IF
     ELSE
         MOVE    "1"        TO   READ-FLG
     END-IF.
     IF (INVALID-FLG     =  "1" )  OR
        (READ-FLG        =  "1" )
         IF  ERR-MSG-CD  =  ZERO
             MOVE  07       TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  HACNO1
             MOVE   "R"     TO   EDIT-OPTION  OF  HACNO2
             MOVE   "C"     TO   EDIT-CURSOR  OF  HACNO1
         END-IF
     END-IF.
 HACNOCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.3     発注ファイル前レコード読込／表示          *
*----------------------------------------------------------*
 HAC-BACK-SUB             SECTION.
     INITIALIZE           TBL-AREA.
     INITIALIZE           OLD-SURYO-AREA.
     MOVE    SPACE        TO   READ-FLG.
     PERFORM    HAC-BACKSTART-SUB.
     IF  EOF-FLG  =  ZERO
         PERFORM    HAC-READ-SUB
         PERFORM    HAC-CHK-SUB
         IF  EOF-FLG  =  ZERO
             MOVE   DSP-REC    TO   SV-DSPREC
             PERFORM    HEADDEL-SUB
             PERFORM    BODYDEL-SUB
             MOVE   HAC-F01    TO  DENKU     SV-DENKU
             MOVE   HAC-F02    TO  HACNO1    SV-HACNO1
             MOVE   HAC-F03    TO  HACNO2    SV-HACNO2
             PERFORM    HAC-SET-SUB
             IF   READ-FLG = "1"
                  MOVE  SV-DSPREC     TO  DSP-REC
                  GO             TO   HAC-BACK-SUB
             END-IF
         ELSE
             MOVE   08           TO   ERR-MSG-CD
             MOVE   ZERO         TO   SV-DENKU
             MOVE   ZERO         TO   SV-HACNO1
             MOVE   ZERO         TO   SV-HACNO2
         END-IF
     ELSE
         MOVE   08           TO   ERR-MSG-CD
         MOVE   ZERO         TO   SV-DENKU
         MOVE   ZERO         TO   SV-HACNO1
         MOVE   ZERO         TO   SV-HACNO2
     END-IF.
 HAC-BACK-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.4     発注ファイル次レコード読込／表示          *
*----------------------------------------------------------*
 HAC-NEXT-SUB             SECTION.
     INITIALIZE           TBL-AREA.
     INITIALIZE           OLD-SURYO-AREA.
     MOVE    SPACE        TO   READ-FLG.
     PERFORM    HAC-NEXTSTART-SUB.
     IF  EOF-FLG  =  ZERO
         PERFORM    HAC-READ-SUB
         PERFORM    HAC-CHK-SUB
         IF  EOF-FLG  =  ZERO
             MOVE   DSP-REC    TO   SV-DSPREC
             PERFORM    HEADDEL-SUB
             PERFORM    BODYDEL-SUB
             MOVE   HAC-F01    TO  DENKU     SV-DENKU
             MOVE   HAC-F02    TO  HACNO1    SV-HACNO1
             MOVE   HAC-F03    TO  HACNO2    SV-HACNO2
             PERFORM    HAC-SET-SUB
             IF   READ-FLG = "1"
                  MOVE  SV-DSPREC     TO  DSP-REC
                  GO             TO   HAC-NEXT-SUB
             END-IF
         ELSE
             MOVE   09           TO   ERR-MSG-CD
             MOVE   99           TO   SV-DENKU
             MOVE   9999999      TO   SV-HACNO1
             MOVE   99           TO   SV-HACNO2
         END-IF
     ELSE
         MOVE   09           TO   ERR-MSG-CD
         MOVE   99           TO   SV-DENKU
         MOVE   9999999      TO   SV-HACNO1
         MOVE   99           TO   SV-HACNO2
     END-IF.
 HAC-NEXT-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.2     発注ファイル読込／表示                    *
*----------------------------------------------------------*
 HACDSP-SUB               SECTION.
     INITIALIZE           TBL-AREA.
     INITIALIZE           OLD-SURYO-AREA.
     PERFORM    HAC-START-SUB.
     IF  EOF-FLG  =  ZERO
         PERFORM    HAC-READ-SUB
         PERFORM    HAC-CHK-SUB
         IF  EOF-FLG  =  ZERO
             PERFORM    HAC-SET-SUB
         ELSE
             MOVE   07           TO   ERR-MSG-CD
         END-IF
     END-IF.
 HACDSP-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.2.1   発注データ画面セット                      *
*----------------------------------------------------------*
 HAC-SET-SUB              SECTION.
     PERFORM         EDIT-SET-SYORI.
     PERFORM         EDIT-SET-HEAD.
     PERFORM         EDIT-SET-BODY.
*\\  93.06.01  START \\\
     MOVE   SPACE        TO   SAV-AREA.
     INITIALIZE               SAV-AREA.
*\\  93.06.01  END   \\\
     PERFORM  UNTIL (EOF-FLG      =  "1"     )  OR
                    (HAC-F01  NOT = SV-DENKU )  OR
                    (HAC-F02  NOT = SV-HACNO1)  OR
                    (HAC-F03  NOT = SV-HACNO2)
         EVALUATE    HAC-F05
             WHEN    00
                 MOVE   HA0-F07      TO   YUBIN
                 MOVE   HA0-F08      TO   OKURI1
                 MOVE   HA0-F09      TO   OKURI2
                 MOVE   HA0-F10      TO   OKURI3
                 MOVE   HA0-F14      TO   TEL
             WHEN    80
                 MOVE   HA0-F11      TO   TEKICD
                 MOVE   HA0-F12      TO   TEKI1
                 MOVE   HA0-F13      TO   TEKI2
             WHEN    99
                 MOVE   HA0-F15      TO   MSGCD
                 MOVE   HA0-F08      TO   WK-MSG1
                 MOVE   HA0-F09      TO   WK-MSG2
                 MOVE   WK-MSG       TO   MSG
             WHEN    OTHER
                IF     HAC-F05      =    1
                    MOVE     01          TO   JYO-F01
                    MOVE     DENKU       TO   JYO-F02
                    PERFORM         JYO-READ-SUB
                    IF  INVALID-FLG   =   ZERO
                        MOVE     JYO-F03    TO   DENKUM
                        REWRITE                  JYO-REC
                    END-IF
                    MOVE   HAC-F12       TO   HACYMD
                    MOVE   HAC-F13       TO   NOUKI
                    MOVE   HAC-F09       TO   TANTO
                    MOVE   HAC-F31       TO   BASYO
                    PERFORM    BSY-READ-SUB
                    MOVE   HAC-F07       TO   RYOHAN
                    MOVE   HAC-F16       TO   SORYOK
                    MOVE   HAC-F17       TO   SORYO
                    MOVE   HAC-F08       TO   SIRCD
                    PERFORM    SHI-READ-SUB
                    MOVE   HAC-F33       TO   SEIKYU
                    MOVE   HAC-F34       TO   HAKKO
                    MOVE   HAC-F15       TO   ZEIKU
                    MOVE   HAC-F18       TO   MEMO
                    MOVE   HAC-F10       TO   TOKCD
                    PERFORM    TOK-READ-SUB
                    IF    (HAC-F01 = 70 OR 71 OR 80 OR 81)
*******             IF    (HAC-F01 = 70 OR 71 OR 80 OR 81) AND
*******                   (HAC-F10(1:5) = 99999)
                        MOVE   ZERO          TO   NONYUC
                        MOVE   HAC-F41       TO   NONYUC(4:2)
                        PERFORM    NYS-READ-SUB
                    ELSE
                        MOVE   HAC-F19       TO   NONYUC
                        PERFORM    TEN-READ-SUB
                    END-IF
                END-IF
                IF    (HAC-F05      >=   1)  AND
                      (HAC-F05      <=   6)
                    MOVE   HAC-F05       TO   IXA
                    MOVE   HAC-F20       TO   SHOCD (IXA)
                    MOVE   HAC-F20       TO   SAV-SHOCD (IXA)
                    MOVE   HAC-F21(1:5)  TO   HINTN1(IXA)
                    MOVE   HAC-F21(1:5)  TO   SAV-HINTN1(IXA)
                    MOVE   HAC-F21(6:2)  TO   HINTN2(IXA)
                    MOVE   HAC-F21(6:2)  TO   SAV-HINTN2(IXA)
                    MOVE   HAC-F21(8:1)  TO   HINTN3(IXA)
                    MOVE   HAC-F21(8:1)  TO   SAV-HINTN3(IXA)
                    MOVE   HAC-F22       TO   SURYO (IXA)
                    MOVE   HAC-F22       TO   SAV-SURYO (IXA)
                    MOVE   HAC-F24       TO   SIRTKB(IXA)
                    MOVE   HAC-F25       TO   SIRTAN(IXA)
                    MOVE   HAC-F26       TO   GENTKB(IXA)
                    MOVE   HAC-F27       TO   GENTAN(IXA)
                    MOVE   HAC-F28       TO   HANTAN(IXA)
                    MOVE   HAC-F29       TO   BIKOU (IXA)
                    MOVE   HAC-F40       TO   TB-FLG(IXA)
                    MOVE   HAC-F22       TO   OLD-SURYO(IXA)
                    MOVE   HAC-F36       TO   OLD-F36(IXA)
                    PERFORM    MEI-READ-SUB
*\\  93.06.01  START \\\
                    MOVE   HAC-F10       TO   SAV-TORICD(IXA)
                    MOVE   HAC-F31       TO   SAV-SOKO  (IXA)
                    MOVE   HAC-F30       TO   SAV-TANA  (IXA)
*\\  93.06.01  END   \\\
                END-IF
         END-EVALUATE
         PERFORM    HAC-READ-SUB
     END-PERFORM.
     PERFORM    GOK-COMP-SUB.
 HAC-SET-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.2.1   発注データ画面セット（合計部              *
*----------------------------------------------------------*
 GOK-COMP-SUB             SECTION.
     MOVE    ZERO             TO   SIRGOK.
     MOVE    ZERO             TO   GENGOK.
     PERFORM  VARYING IXA     FROM  1  BY  1
              UNTIL   IXA     >     6
         IF  (SHOCD(IXA)  =  SPACE    )  AND
             (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             IF  SIRTKB(IXA) = SPACE OR "1"
                 COMPUTE WK-KIN  =  SURYO(IXA) * SIRTAN(IXA)
                 ADD     WK-KIN       TO  SIRGOK
             ELSE
                 ADD     SIRTAN(IXA)  TO  SIRGOK
             END-IF
             IF  GENTKB(IXA) = SPACE OR "1"
                 COMPUTE WK-KIN  =  SURYO(IXA) * GENTAN(IXA)
                 ADD     WK-KIN       TO  GENGOK
             ELSE
                 ADD     GENTAN(IXA)  TO  GENGOK
             END-IF
         END-IF
     END-PERFORM.
 GOK-COMP-END.
     EXIT.
*==========================================================*
*      2.3       ヘッド部入力              MAIN-FLG=3      *
*==========================================================*
 HEAD-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG03         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     IF   WK-SYORI  =    1
         MOVE    "HEAD1"     TO   DSP-GROUP
     ELSE
         MOVE    "HEAD2"     TO   DSP-GROUP
     END-IF.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                        IF   WK-SYORI  =    1
                             MOVE   "1"     TO   MAIN-FLG
                        ELSE
                             MOVE   "2"     TO   MAIN-FLG
                        END-IF
         WHEN   "E000"
                        PERFORM   HEADCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "4"     TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      2.3.1     ヘッド部の入力チェック                    *
*----------------------------------------------------------*
 HEADCHK-SUB             SECTION.
     PERFORM         EDIT-SET-HEAD.
     MOVE       ZERO    TO   ERR-MSG-CD.
****  伝区のチェック  ****
     IF   WK-SYORI  =    1
         IF  DENKU     NOT  NUMERIC
             MOVE   ZERO         TO   DENKU
         END-IF
         MOVE     SPACE          TO   DENKUM
         MOVE     01             TO   JYO-F01
         MOVE     DENKU          TO   JYO-F02
         PERFORM         JYO-READ-SUB
         IF  INVALID-FLG   =   ZERO
             MOVE     JYO-F03    TO   DENKUM
             REWRITE                  JYO-REC
         ELSE
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DENKU
             MOVE   "C"     TO   EDIT-CURSOR  OF  DENKU
         END-IF
     END-IF.
*93.10.21
****  計上フラグのチェック  ****
     IF  KEIJYO        NOT =     ZERO AND  1
         MOVE   03     TO   ERR-MSG-CD
         MOVE   "R"    TO   EDIT-OPTION  OF  KEIJYO
         MOVE   "C"    TO   EDIT-CURSOR  OF  KEIJYO
     ELSE
         IF   KEIJYO            =     1
              IF  (DENKU         NOT =     50)    OR
                  (WK-SYORI      NOT =     1 )
                   MOVE   03     TO   ERR-MSG-CD
                   MOVE   "R"    TO   EDIT-OPTION  OF  KEIJYO
                   MOVE   "C"    TO   EDIT-CURSOR  OF  KEIJYO
              END-IF
         END-IF
     END-IF.
****  発注日のチェック  ****
     IF (HACYMD    NOT  NUMERIC)  OR
        (HACYMD    =    ZERO   )
         MOVE   03      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  HACYMD
         MOVE   "C"     TO   EDIT-CURSOR  OF  HACYMD
     ELSE
         MOVE   HACYMD      TO   CHK-YMD
         PERFORM    YMDCHK-SUB
         IF  INVALID-FLG   =   "1"
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  HACYMD
             MOVE   "C"     TO   EDIT-CURSOR  OF  HACYMD
         ELSE
             MOVE   LINK-OUT-YMD  TO NEW-HACYMD
         END-IF
     END-IF.
****  納期のチェック  ****
     IF (NOUKI     NOT  NUMERIC)  OR
        (NOUKI     =    ZERO   )
         IF  EDIT-OPTION  OF  HACYMD  NOT =  "R"
             MOVE   HACYMD      TO   CHK-YMD
             PERFORM    NOUKI-SUB
*## 1999/12/20 NAV
             MOVE   LINK-OUT-YMD  TO NEW-NOUKI
         END-IF
     ELSE
         MOVE   NOUKI       TO   CHK-YMD
         PERFORM    YMDCHK-SUB
*## 1999/12/20 NAV
         MOVE   LINK-OUT-YMD  TO NEW-NOUKI
*********IF (INVALID-FLG   =   ZERO)  AND  (HACYMD > NOUKI )
         IF (INVALID-FLG   =   ZERO)
         AND (NEW-HACYMD > NEW-NOUKI)
             MOVE   "1"     TO   INVALID-FLG
         END-IF
         IF  INVALID-FLG   =   "1"
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  NOUKI
             MOVE   "C"     TO   EDIT-CURSOR  OF  NOUKI
         END-IF
     END-IF.
*93.10.21
     MOVE    NOUKI      TO        WK-NOUHIN.
     IF      NOUKI(1:2) >         "90"
             ADD      19000000    TO WK-NOUHIN
     ELSE
             ADD      20000000    TO WK-NOUHIN
     END-IF.
****  受付担当のチェック  ****
     IF  TANTO     NOT  NUMERIC
         MOVE   03      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  TANTO
         MOVE   "C"     TO   EDIT-CURSOR  OF  TANTO
     END-IF.
****  場所のチェック  ****
     IF  BASYO     =    LOW-VALUE
         MOVE    SPACE          TO  BASYO
     END-IF.
     MOVE    SPACE              TO  BASYOM.
     EVALUATE    TRUE
         WHEN    DENKU  =  50  OR  51
             IF  BASYO     =    SPACE
                 MOVE   03      TO   ERR-MSG-CD
                 MOVE   "R"     TO   EDIT-OPTION  OF  BASYO
                 MOVE   "C"     TO   EDIT-CURSOR  OF  BASYO
             ELSE
                 PERFORM    BSY-READ-SUB
                 IF  INVALID-FLG   =   "1"
                     MOVE   03      TO   ERR-MSG-CD
                     MOVE   "R"     TO   EDIT-OPTION  OF  BASYO
                     MOVE   "C"     TO   EDIT-CURSOR  OF  BASYO
                 END-IF
             END-IF
         WHEN    DENKU  =  60  OR  61
             MOVE   "79"        TO   BASYO
             PERFORM    BSY-READ-SUB
         WHEN    OTHER
             IF  BASYO     =    SPACE
                 CONTINUE
             ELSE
                 PERFORM    BSY-READ-SUB
                 IF  INVALID-FLG   =   "1"
                     MOVE   03      TO   ERR-MSG-CD
                     MOVE   "R"     TO   EDIT-OPTION  OF  BASYO
                     MOVE   "C"     TO   EDIT-CURSOR  OF  BASYO
                 END-IF
             END-IF
     END-EVALUATE.
****  送料区分のチェック  ****
     IF  SORYOK  NOT NUMERIC
         MOVE    ZERO        TO  SORYOK
     END-IF.
     IF  SORYOK  =  0  OR  8  OR  9
         CONTINUE
     ELSE
         MOVE   03      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  SORYOK
         MOVE   "C"     TO   EDIT-CURSOR  OF  SORYOK
     END-IF.
     IF  SORYO   NOT NUMERIC
         MOVE    ZERO        TO  SORYO
     END-IF.
****  仕入先のチェック  ****
     MOVE   SPACE           TO   SIRNM.
     IF (SIRCD     =    LOW-VALUE) OR
        (SIRCD     =    SPACE    )
         MOVE   SPACE       TO   SIRCD
     ELSE
         PERFORM    SHI-READ-SUB
         IF  INVALID-FLG   =   "1"
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
             MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
         END-IF
     END-IF
****  請求のチェック  ****
     IF (SEIKYU  NUMERIC    )  AND
        (SEIKYU  =  0  OR  9)
         CONTINUE
     ELSE
         MOVE   03      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  SEIKYU
         MOVE   "C"     TO   EDIT-CURSOR  OF  SEIKYU
     END-IF.
****  発行のチェック  ****
     IF (HAKKO   NUMERIC    )  AND
        (HAKKO   =  0  OR  9)
         CONTINUE
     ELSE
         MOVE   03      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  HAKKO
         MOVE   "C"     TO   EDIT-CURSOR  OF  HAKKO
     END-IF.
****  税区のチェック  ****
     IF (ZEIKU   NUMERIC    )  AND
        (ZEIKU   =  0  OR  2  OR  8  OR  9)
         CONTINUE
     ELSE
         MOVE   03      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  ZEIKU
         MOVE   "C"     TO   EDIT-CURSOR  OF  ZEIKU
     END-IF.
****  メモ  ****
     IF  MEMO      =    LOW-VALUE
         MOVE   SPACE       TO   MEMO
     END-IF.
****  得意先／量販_のチェック  ****
     IF  TOKCD   NOT NUMERIC
         MOVE    ZERO        TO  TOKCD
     END-IF.
     IF  RYOHAN  NOT NUMERIC
         MOVE    ZERO        TO  RYOHAN
     END-IF.
     IF  TOKCD   =  ZERO
       IF (DENKU = 60 OR 61 OR 70 OR 71 OR 80 OR 81 )         OR
            (SIRCD = "29099999"                       )
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
             MOVE   "C"     TO   EDIT-CURSOR  OF  TOKCD
       END-IF
         MOVE    SPACE       TO  TOKNM
     ELSE
         PERFORM    TOK-READ-SUB
         IF  INVALID-FLG   =   "1"
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
             MOVE   "C"     TO   EDIT-CURSOR  OF  TOKCD
         ELSE
             IF  SIRCD            =  "29099999"
                 IF   TOKCD(1:5)  =  "99999"
                     CONTINUE
                 ELSE
                     MOVE   03      TO   ERR-MSG-CD
                     MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
                     MOVE   "C"     TO   EDIT-CURSOR  OF  TOKCD
                 END-IF
             END-IF
*\\          93.06.23
*************IF (DENKU       = 60  ) AND
             IF (RYOHAN  NOT = ZERO)
                 MOVE    TOK-F94       TO  LI-KBN
                 MOVE    TOK-F92       TO  LI-KETA
                 MOVE    TOK-F60       TO  LI-START
                 MOVE    TOK-F61       TO  LI-END
                 MOVE    RYOHAN        TO  LI-DENNO
                 CALL    "OSKTCDCK"    USING   LINK-AREA
                 IF    TOKCD  NOT =  2363
                   IF  LO-ERR   NOT =  ZERO
                     MOVE   03         TO   ERR-MSG-CD
                     MOVE   "R"        TO   EDIT-OPTION  OF RYOHAN
                     MOVE   "C"        TO   EDIT-CURSOR  OF RYOHAN
                   END-IF
                 END-IF
              END-IF
         END-IF
     END-IF.
****  納入先のチェック  ****
     IF  NONYUC  NOT NUMERIC
         MOVE    ZERO        TO  NONYUC
     END-IF.
     MOVE   SPACE            TO  NONYUM.
     IF  NONYUC  NOT =  ZERO
*\   93.06.23
*********IF     DENKU = 50
*********    MOVE   03      TO   ERR-MSG-CD
*********    MOVE   "R"     TO   EDIT-OPTION  OF  NONYUC
*********    MOVE   "C"     TO   EDIT-CURSOR  OF  NONYUC
*********END-IF
         IF     TOKCD  NUMERIC
*******      IF    (DENKU  =  70 OR 71 OR 80 OR 81) AND
*******            (SIRCD  =  "29099999"           )
             IF    (DENKU  =  70 OR 71 OR 80 OR 81)

                 PERFORM    NYS-READ-SUB
             ELSE
                 PERFORM    TEN-READ-SUB
             END-IF
             IF  INVALID-FLG   =   "1"
                 MOVE   03      TO   ERR-MSG-CD
                 MOVE   "R"     TO   EDIT-OPTION  OF  NONYUC
                 MOVE   "C"     TO   EDIT-CURSOR  OF  NONYUC
             END-IF
         ELSE
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  NONYUC
             MOVE   "C"     TO   EDIT-CURSOR  OF  NONYUC
         END-IF
     END-IF.
****  送先名区分のチェック  ****
     IF  SOFUKB    =    LOW-VALUE
         MOVE    SPACE          TO  SOFUKB
     END-IF.
     IF  OKURI1    =    LOW-VALUE
         MOVE    SPACE          TO  OKURI1
     END-IF.
     IF  OKURI2    =    LOW-VALUE
         MOVE    SPACE          TO  OKURI2
     END-IF.
     IF  OKURI3    =    LOW-VALUE
         MOVE    SPACE          TO  OKURI3
     END-IF.
     EVALUATE    SOFUKB
         WHEN    SPACE
             IF  (YUBIN  = SPACE) AND
                 (OKURI1 = SPACE) AND
                 (OKURI2 = SPACE) AND
                 (OKURI3 = SPACE)
                 CONTINUE
             ELSE
                 MOVE   03      TO   ERR-MSG-CD
                 MOVE   "R"     TO   EDIT-OPTION  OF  SOFUKB
                 MOVE   "C"     TO   EDIT-CURSOR  OF  SOFUKB
             END-IF
         WHEN    "1"
             IF ((YUBIN  = SPACE) AND
                 (OKURI1 = SPACE) AND
                 (OKURI2 = SPACE) AND
                 (OKURI3 = SPACE)     )  OR
                ( TOKCD  = ZERO )
                 MOVE   03      TO   ERR-MSG-CD
                 MOVE   "R"     TO   EDIT-OPTION  OF  YUBIN
                 MOVE   "R"     TO   EDIT-OPTION  OF  OKURI1
                 MOVE   "R"     TO   EDIT-OPTION  OF  OKURI2
                 MOVE   "R"     TO   EDIT-OPTION  OF  OKURI3
                 MOVE   "C"     TO   EDIT-CURSOR  OF  SOFUKB
             END-IF
         WHEN    OTHER
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  SOFUKB
             MOVE   "C"     TO   EDIT-CURSOR  OF  SOFUKB
     END-EVALUATE.
****  社外ＭＳＧのチェック  ****
     IF  MSGCD     =    LOW-VALUE
         MOVE    SPACE          TO  MSGCD
     END-IF.
     IF  MSG       =    LOW-VALUE
         MOVE    SPACE          TO  MSG
     END-IF.
     IF  MSG  =  SPACE
         IF  MSGCD  NOT = SPACE
             MOVE     98             TO   JYO-F01
             MOVE     MSGCD          TO   JYO-F02
             PERFORM         JYO-READ-SUB
             IF  INVALID-FLG   =   ZERO
                 MOVE     JYO-F03    TO   MSG
                 REWRITE                  JYO-REC
             ELSE
                 MOVE   03      TO   ERR-MSG-CD
                 MOVE   "R"     TO   EDIT-OPTION  OF  MSGCD
                 MOVE   "C"     TO   EDIT-CURSOR  OF  MSGCD
             END-IF
         END-IF
     END-IF.
 HEADCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.3.1      納期省略時自動算出                       *
*----------------------------------------------------------*
 NOUKI-SUB              SECTION.
*  閏年判定
*****PERFORM URUCHK-SUB.
** 93.05.31  START  ***
***発注日＋３
**** ADD     3                   TO    CHK-DD.
*  発注日＋１
*    ADD     1                   TO    CHK-DD.
*
** 93.05.31  END    ***
***  IF      CHK-DD              >     DAY-T(CHK-MM)
***      COMPUTE CHK-DD          =     CHK-DD  -   DAY-T(CHK-MM)
***      ADD     1               TO    CHK-MM
***      IF      CHK-MM          >     12
***          MOVE    1           TO    CHK-MM
***          ADD     1           TO    CHK-YY
***      END-IF
***  END-IF.
***  MOVE    CHK-YMD             TO    NOUKI.
*## 1999/12/20 NAV 日付判定方法変更サブルーチン使用
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     CHK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
*## ＋１日算出
     MOVE     "5"                 TO   LINK-IN-KBN.
     MOVE     1                   TO   LINK-IN-YMD6.
     MOVE     LINK-OUT-YMD        TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD     TO      NOUKI.
 NOUKI-END.
     EXIT.
*==========================================================*
*      2.4       ボディ部入力              MAIN-FLG=4      *
*==========================================================*
 BODY-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG04         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE   "BODY"           TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
         WHEN   "F006"
                        MOVE    "3"    TO   MAIN-FLG
         WHEN   "E000"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "5"     TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 BODY-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ボディ部の入力チェック                    *
*----------------------------------------------------------*
 BODYCHK-SUB             SECTION.
     PERFORM         EDIT-SET-BODY.
     MOVE       ZERO    TO   ERR-MSG-CD.
*
     PERFORM  VARYING IXA     FROM  1  BY  1
              UNTIL   IXA     >     6
       IF   SHOCD (IXA) =  LOW-VALUE
            MOVE   SPACE      TO   SHOCD (IXA)
       END-IF
       IF   HINTN1(IXA) =  LOW-VALUE
            MOVE   SPACE      TO   HINTN1(IXA)
       END-IF
       IF   HINTN2(IXA) =  LOW-VALUE
            MOVE   SPACE      TO   HINTN2(IXA)
       END-IF
       IF   HINTN3(IXA) =  LOW-VALUE
            MOVE   SPACE      TO   HINTN3(IXA)
       END-IF
*** 品単　右詰処理 ***
       PERFORM VARYING IXB    FROM  1   BY   1
               UNTIL   IXB    >     4
           PERFORM VARYING IXC    FROM  5   BY  -1
                   UNTIL   IXC    <     2
             IF  HINTN1(IXA)(IXC:1)   =  SPACE
                 COMPUTE IXD    =     IXC   -   1
                 MOVE  HINTN1(IXA)(IXD:1)   TO  HINTN1(IXA)(IXC:1)
                 MOVE  SPACE                TO  HINTN1(IXA)(IXD:1)
             END-IF
           END-PERFORM
       END-PERFORM
*** 商品コード右詰処理  ***
       IF      SHOCD(IXA)     NOT =     SPACE
            PERFORM VARYING IXB    FROM  1   BY   1
                    UNTIL   IXB    >     7
                PERFORM VARYING IXC    FROM  8   BY  -1
                        UNTIL   IXC    <     2
                  IF  SHOCD (IXA)(IXC:1)   =  SPACE
                      COMPUTE IXD    =     IXC   -   1
                      MOVE  SHOCD (IXA)(IXD:1)   TO
                                                SHOCD (IXA)(IXC:1)
                      MOVE  SPACE                TO
                                                SHOCD (IXA)(IXD:1)
                  END-IF
              END-PERFORM
            END-PERFORM
**
            PERFORM     VARYING    IXB   FROM    1   BY   1
                        UNTIL      (IXB   >      7 ) OR
                        (SHOCD(IXA)(IXB:1) NOT =  SPACE)
              IF        SHOCD(IXA)(IXB:1)        =   SPACE
                        MOVE       "0"   TO      SHOCD(IXA)(IXB:1)
              END-IF
            END-PERFORM
       END-IF
**
**
       IF (  SHOCD(IXA)  =  SPACE    )  AND
          ( (SURYO(IXA)  NOT  NUMERIC)  OR  (SURYO(IXA) = ZERO) )
         MOVE    SPACE        TO    BODY1(IXA)
         MOVE    IXA          TO    IXB
         PERFORM EDIT-SET-GYO
       ELSE
****  商品コード／品単のチェック  ****
         INITIALIZE          MEI-REC
         IF   SHOCD(IXA)  =  SPACE
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(IXA)
             MOVE   "C"     TO   EDIT-CURSOR  OF  SHOCD(IXA)
         ELSE
             PERFORM   MEI-READ-SUB
             IF  INVALID-FLG   =   "1"
                 MOVE   03   TO   ERR-MSG-CD
                 MOVE   "R"  TO   EDIT-OPTION  OF  SHOCD (IXA)
                 MOVE   "R"  TO   EDIT-OPTION  OF  HINTN1(IXA)
                 MOVE   "R"  TO   EDIT-OPTION  OF  HINTN2(IXA)
                 MOVE   "R"  TO   EDIT-OPTION  OF  HINTN3(IXA)
                 MOVE   "C"  TO   EDIT-CURSOR  OF  SHOCD (IXA)
             ELSE
                 MOVE   MEI-F043     TO   HANTAN(IXA)
                 IF     SIRCD  =  SPACE
                     MOVE    MEI-F05      TO   SIRCD
                     PERFORM    SHI-READ-SUB
                 END-IF
             END-IF
         END-IF
****  数量のチェック  ****
         IF  SURYO(IXA)  NOT  NUMERIC
             MOVE   ZERO         TO  SURYO(IXA)
         END-IF
         IF  MEI-F07  NOT =  ZERO
             DIVIDE  SURYO(IXA)  BY  MEI-F07  GIVING    CHK-SURYO1
                                              REMAINDER CHK-SURYO2
             IF  CHK-SURYO2 NOT = ZERO
                 MOVE   "R"     TO   EDIT-OPTION  OF  SURYO(IXA)
             END-IF
         END-IF
****  仕単／金額のチェック  ****
         IF  SIRTAN(IXA) NOT  NUMERIC
             MOVE   ZERO         TO  SIRTAN(IXA)
         END-IF
         IF  SIRTKB(IXA)  =  SPACE
             MOVE   MEI-F041    TO   SIRTAN(IXA)
                 IF  SIRTAN(IXA) =    ZERO
                     MOVE   03   TO   ERR-MSG-CD
                     MOVE   "R"  TO   EDIT-OPTION  OF  SIRTAN(IXA)
                     MOVE   "C"  TO   EDIT-CURSOR  OF  SIRTAN(IXA)
                 END-IF
         ELSE
             IF  SIRTKB(IXA)  =  "5" OR "6" OR "7"
                 IF  SIRTAN(IXA) NOT = ZERO
                     MOVE   03   TO   ERR-MSG-CD
                     MOVE   "R"  TO   EDIT-OPTION  OF  SIRTAN(IXA)
                     MOVE   "C"  TO   EDIT-CURSOR  OF  SIRTAN(IXA)
                 END-IF
             END-IF
         END-IF
****  仕入単価区分のチェック  ****
         IF  SIRTKB(IXA)  =  SPACE OR "1" OR "3" OR "4" OR "5"
                                   OR "6" OR "7"
             CONTINUE
         ELSE
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  SIRTKB(IXA)
             MOVE   "C"     TO   EDIT-CURSOR  OF  SIRTKB(IXA)
         END-IF
****  原価単価区分のチェック  ****
         IF  GENTKB(IXA)  =  SPACE OR "1" OR "3" OR "4" OR "5"
                                   OR "6" OR "7"
             CONTINUE
         ELSE
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  GENTKB(IXA)
             MOVE   "C"     TO   EDIT-CURSOR  OF  GENTKB(IXA)
         END-IF
****  原単／金額のチェック  ****
         IF  GENTAN(IXA) NOT  NUMERIC
             MOVE   ZERO         TO  GENTAN(IXA)
         END-IF
         IF  GENTKB(IXA)  =  SPACE
             MOVE   MEI-F042    TO   GENTAN(IXA)
         ELSE
             IF  GENTKB(IXA)  =  "5" OR "6" OR "7"
                 IF  GENTAN(IXA) NOT = ZERO
                     MOVE   03   TO   ERR-MSG-CD
                     MOVE   "R"  TO   EDIT-OPTION  OF  GENTAN(IXA)
                     MOVE   "C"  TO   EDIT-CURSOR  OF  GENTAN(IXA)
                 END-IF
             END-IF
         END-IF
       END-IF
     END-PERFORM.
****  摘要コード／摘要のチェック  ****
     IF  TEKICD   NOT  NUMERIC
         MOVE   ZERO         TO  TEKICD
     END-IF.
     IF  TEKICD  =  ZERO
         MOVE   SPACE        TO  TEKI1
         MOVE   SPACE        TO  TEKI2
     ELSE
         IF  TEKICD  =  99
            IF  (TEKI1   =  SPACE)  AND
                (TEKI2   =  SPACE)
                 MOVE   03   TO   ERR-MSG-CD
                 MOVE   "R"  TO   EDIT-OPTION  OF  TEKI1
                 MOVE   "R"  TO   EDIT-OPTION  OF  TEKI2
                 MOVE   "C"  TO   EDIT-CURSOR  OF  TEKI1
            END-IF
         ELSE
             MOVE     80             TO   JYO-F01
             MOVE     TEKICD         TO   JYO-F02
             PERFORM         JYO-READ-SUB
             IF  INVALID-FLG   =   ZERO
                 MOVE     JYO-F14    TO   TEKI1
                 MOVE     JYO-F15    TO   TEKI2
                 REWRITE                  JYO-REC
             ELSE
                 MOVE   03      TO   ERR-MSG-CD
                 MOVE   "R"     TO   EDIT-OPTION  OF  TEKICD
                 MOVE   "C"     TO   EDIT-CURSOR  OF  TEKICD
             END-IF
         END-IF
     END-IF.
****  削除行処理  ****
     IF  ERR-MSG-CD  =  ZERO
         IF  WK-SYORI  =  2
             INITIALIZE   DLT-TABLE
             PERFORM  VARYING IXA     FROM  1  BY  1
                      UNTIL   IXA     >     5
                 IF  (SURYO (IXA)     NUMERIC)  AND
                     (SURYO (IXA)     = ZERO )  AND
                     (TB-FLG(IXA)     = 1    )
                     MOVE   SPACE            TO  BODY1(IXA)
                     MOVE   IXA              TO  IXB
                     PERFORM EDIT-SET-GYO
                 END-IF
                 IF  (SURYO (IXA)     NUMERIC)  AND
                     (SURYO (IXA)     = ZERO )  AND
                     (TB-FLG(IXA) NOT = 1    )
                     PERFORM VARYING IXB     FROM  IXA  BY  1
                             UNTIL   IXB     >     5
                         COMPUTE IXC      =    IXB  +  1
                         MOVE  SHOCD    (IXB)  TO  DLT-SHOCD(IXB)
                         MOVE  HINTN1   (IXB)  TO  DLT-HINTN1(IXB)
                         MOVE  HINTN2   (IXB)  TO  DLT-HINTN2(IXB)
                         MOVE  HINTN3   (IXB)  TO  DLT-HINTN3(IXB)
                         MOVE  OLD-SURYO(IXB)  TO  DLT-SURYO(IXB)
                         MOVE  OLD-F36  (IXB)  TO  DLT-F36  (IXB)
                         MOVE  BODY1    (IXC)  TO  BODY1    (IXB)
                         MOVE  OLD-SURYO(IXC)  TO  OLD-SURYO(IXB)
                         MOVE  OLD-F36  (IXC)  TO  OLD-F36  (IXB)
                         MOVE  SPACE           TO  BODY1    (IXC)
                         MOVE  ZERO            TO  OLD-SURYO(IXC)
                         MOVE  ZERO            TO  TB-FLG   (IXC)
                         PERFORM EDIT-SET-GYO
                     END-PERFORM
                 END-IF
             END-PERFORM
         END-IF
     END-IF.
****  全行未入力チェック／合計計算  ****
     IF  ERR-MSG-CD  =  ZERO
         IF  ( SHOCD(1) = SPACE )  AND
             ( SHOCD(2) = SPACE )  AND
             ( SHOCD(3) = SPACE )  AND
             ( SHOCD(4) = SPACE )  AND
             ( SHOCD(5) = SPACE )  AND
             ( SHOCD(6) = SPACE )
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(1)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(2)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(3)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(4)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(5)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(6)
             MOVE   "C"     TO   EDIT-CURSOR  OF  SHOCD(1)
         ELSE
             PERFORM        GOK-COMP-SUB
         END-IF
     END-IF.
 BODYCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力                                  *
*----------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG04         TO   PFMSG.
     MOVE     "Y"            TO   ANS.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "KAKU"          TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
         WHEN   "F006"
                        IF  WK-SYORI  =  3  OR  9
                            MOVE  "2"       TO   MAIN-FLG
                        ELSE
                            MOVE  "4"       TO   MAIN-FLG
                        END-IF
         WHEN   "E000"
                        PERFORM   KAKUCHK-SUB
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1     確認の入力チェック                        *
*----------------------------------------------------------*
 KAKUCHK-SUB             SECTION.
     MOVE       "M"     TO   EDIT-OPTION  OF  ANS.
     MOVE       SPACE   TO   EDIT-CURSOR  OF  ANS.
     MOVE       ZERO    TO   ERR-MSG-CD.
*
     EVALUATE  ANS
         WHEN   "Y"
              PERFORM                 FILUPDT-SUB
              PERFORM                 HEADDEL-SUB
              PERFORM                 BODYDEL-SUB
              IF  WK-SYORI  =  1
                  PERFORM             HACNO-SET-SUB
                  PERFORM             HACYMD-SET-SUB
                  MOVE  "3"       TO  MAIN-FLG
              ELSE
                  MOVE  "2"       TO  MAIN-FLG
              END-IF
         WHEN   "H"
              IF  WK-SYORI  =  1
                  PERFORM             FILUPDT-SUB
                  PERFORM             BODYDEL-SUB
                  PERFORM             HACNO-SET-SUB
                  MOVE  "3"       TO  MAIN-FLG
              END-IF
              IF  WK-SYORI  =  2  OR  3
                  PERFORM             FILUPDT-SUB
                  PERFORM             HAC-NEXT-SUB
                  MOVE  "2"       TO  MAIN-FLG
              END-IF
              IF  WK-SYORI  =  9
                  MOVE   06   TO   ERR-MSG-CD
                  MOVE   "R"  TO   EDIT-OPTION  OF  ANS
                  MOVE   "C"  TO   EDIT-CURSOR  OF  ANS
              END-IF
         WHEN   "B"
              IF  WK-SYORI  =  1
                  PERFORM             FILUPDT-SUB
                  PERFORM             HEADDEL-SUB
                  PERFORM             HACNO-SET-SUB
                  PERFORM             HACYMD-SET-SUB
                  MOVE  "3"       TO  MAIN-FLG
              ELSE
                  IF  WK-SYORI  =  2  OR  3
                      MOVE   05   TO   ERR-MSG-CD
                  ELSE
                      MOVE   06   TO   ERR-MSG-CD
                  END-IF
                  MOVE   "R"  TO   EDIT-OPTION  OF  ANS
                  MOVE   "C"  TO   EDIT-CURSOR  OF  ANS
              END-IF
         WHEN   OTHER
              MOVE   04   TO   ERR-MSG-CD
              MOVE   "R"  TO   EDIT-OPTION  OF  ANS
              MOVE   "C"  TO   EDIT-CURSOR  OF  ANS
     END-EVALUATE.
 KAKUCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1    ファイル更新                             *
*----------------------------------------------------------*
 FILUPDT-SUB            SECTION.
     EVALUATE  WK-SYORI
         WHEN  1
              PERFORM             HACHDT-OUT-SUB
              IF   DENKU = 50 OR 60
*93.10.21 START
                   IF  KEIJYO =   1
                       PERFORM    NYUKDT-OUT-SUB
                   ELSE
                       CONTINUE
                   END-IF
*93.10.21 END
              ELSE
                  PERFORM         NYUKDT-OUT-SUB
              END-IF
         WHEN  2
              PERFORM             HACHDT-UPD-SUB
              IF   DENKU = 50 OR 60
                   IF  KEIJYO =   1
                       PERFORM    NYUKDT-UPD-SUB
                   ELSE
                       CONTINUE
                   END-IF
              ELSE
                  PERFORM         NYUKDT-UPD-SUB
              END-IF
         WHEN  3
              PERFORM             HACHDT-DLT-SUB
              IF   DENKU = 50 OR 60
                   IF  KEIJYO =   1
                       PERFORM    NYUKDT-DLT-SUB
                   ELSE
                       CONTINUE
                   END-IF
              ELSE
                  PERFORM         NYUKDT-DLT-SUB
              END-IF
     END-EVALUATE.
     IF    ( DENKU = 50 OR 51 ) OR
           ((DENKU = 70 OR 71 OR 80 OR 81) AND
           ( SIRCD = "29099999")  )
          PERFORM         ZAIMS-UPD-SUB
     END-IF.
 FILUPDT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.1  発注ファイル登録／本日分出力             *
*----------------------------------------------------------*
 HACHDT-OUT-SUB         SECTION.
*  行＝００
     IF    ( YUBIN  = SPACE ) AND
           ( OKURI1 = SPACE ) AND
           ( OKURI2 = SPACE ) AND
           ( OKURI3 = SPACE ) AND
           ( TEL    = SPACE )
         CONTINUE
     ELSE
         MOVE    SPACE           TO   HAC0-REC
         INITIALIZE                   HAC0-REC
         MOVE    DENKU           TO   HA0-F01
         MOVE    HACNO1          TO   HA0-F02
         MOVE    HACNO2          TO   HA0-F03
         MOVE    ZERO            TO   HAC-F05
         PERFORM   HACHDT-EDIT00-SUB
*## 1999/12/20 NAV ##*
*********COMPUTE HA0-F98  =  SYS-DATE + SEIREKI
         MOVE    SYS-DATE2   TO    HA0-F98
         WRITE                        HAC-REC
         WRITE   HWK-REC         FROM HAC-REC
     END-IF.
*  行＝０１～０６
     MOVE    SPACE           TO   HAC1-REC.
     INITIALIZE                   HAC1-REC.
     MOVE    DENKU           TO   HAC-F01.
     MOVE    HACNO1          TO   HAC-F02.
     MOVE    HACNO2          TO   HAC-F03.
     MOVE    ZERO            TO   HAC-F05.
*## 1999/12/20 NAV
*****COMPUTE HAC-F98  =  SYS-DATE + SEIREKI.
     MOVE    SYS-DATE2       TO   HAC-F98.
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             ADD     1               TO   HAC-F05
             PERFORM   HACHDT-EDIT-SUB
             WRITE                        HAC-REC
             WRITE   HWK-REC         FROM HAC-REC
         END-IF
     END-PERFORM.
*  行＝８０
     IF    ( TEKICD = ZERO  ) AND
           ( TEKI1  = SPACE ) AND
           ( TEKI2  = SPACE )
         CONTINUE
     ELSE
         MOVE    SPACE           TO   HAC0-REC
         INITIALIZE                   HAC0-REC
         MOVE    DENKU           TO   HA0-F01
         MOVE    HACNO1          TO   HA0-F02
         MOVE    HACNO2          TO   HA0-F03
         MOVE    80              TO   HA0-F05
         PERFORM   HACHDT-EDIT80-SUB
*## 1999/12/20 NAV
*********COMPUTE HA0-F98  =  SYS-DATE + SEIREKI
         MOVE    SYS-DATE2       TO   HA0-F98
         WRITE                        HAC-REC
         WRITE   HWK-REC         FROM HAC-REC
     END-IF.
*  行＝９９
     IF    ( MSGCD  = SPACE ) AND
           ( MSG    = SPACE )
         CONTINUE
     ELSE
         MOVE    SPACE           TO   HAC0-REC
         INITIALIZE                   HAC0-REC
         MOVE    DENKU           TO   HA0-F01
         MOVE    HACNO1          TO   HA0-F02
         MOVE    HACNO2          TO   HA0-F03
         MOVE    99              TO   HA0-F05
         PERFORM   HACHDT-EDIT99-SUB
*## 1999/12/20 NAV
*********COMPUTE HA0-F98  =  SYS-DATE + SEIREKI
         MOVE    SYS-DATE2       TO     HA0-F98
         WRITE                        HAC-REC
         WRITE   HWK-REC         FROM HAC-REC
     END-IF.
 HACHDT-OUT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.1.1  発注ファイル編集（行＝０１～０６）     *
*----------------------------------------------------------*
 HACHDT-EDIT-SUB        SECTION.
     IF      DENKU = 50 OR 60
*93.10.21
         IF  KEIJYO     =  1
             MOVE    1      TO   HAC-F06
             MOVE    1      TO   HAC-F42
         ELSE
             MOVE    ZERO   TO   HAC-F06
         END-IF
     ELSE
         MOVE    1           TO   HAC-F06
     END-IF.
     MOVE    RYOHAN          TO   HAC-F07.
     MOVE    SIRCD           TO   HAC-F08.
     MOVE    TANTO           TO   HAC-F09.
     MOVE    TOKCD           TO   HAC-F10.
     PERFORM    TOK-READ-SUB
     IF  INVALID-FLG   =   ZERO
         MOVE    TOK-F52     TO   HAC-F11
     ELSE
         MOVE    ZERO        TO   HAC-F11
     END-IF.
*## 1999/12/20 NAV
*****COMPUTE HAC-F12  =  HACYMD + SEIREKI.
*****COMPUTE HAC-F13  =  NOUKI  + SEIREKI.
     MOVE    NEW-HACYMD      TO   HAC-F12.
     MOVE    NEW-NOUKI       TO   HAC-F13.
     MOVE    ZEIKU           TO   HAC-F15.
     MOVE    SORYOK          TO   HAC-F16.
     MOVE    SORYO           TO   HAC-F17.
     MOVE    MEMO            TO   HAC-F18.
*****IF  (DENKU = 70 OR 71 OR 80 OR 81) AND
*****    (TOKCD(1:5) = 99999)
     IF  (DENKU = 70 OR 71 OR 80 OR 81)

         MOVE    ZERO        TO   HAC-F19
         MOVE    NONYUC(4:2) TO   HAC-F41
     ELSE
         MOVE    NONYUC      TO   HAC-F19
         MOVE    SPACE       TO   HAC-F41
     END-IF.
     MOVE    SHOCD (IXA)     TO   HAC-F20.
     MOVE    HINTN1(IXA)     TO   HAC-F21(1:5).
     MOVE    HINTN2(IXA)     TO   HAC-F21(6:2).
     MOVE    HINTN3(IXA)     TO   HAC-F21(8:1).
     MOVE    SURYO (IXA)     TO   HAC-F22.
     MOVE    ZERO            TO   HAC-F23.
     MOVE    SIRTKB(IXA)     TO   HAC-F24.
     MOVE    SIRTAN(IXA)     TO   HAC-F25.
     MOVE    GENTKB(IXA)     TO   HAC-F26.
     MOVE    GENTAN(IXA)     TO   HAC-F27.
     MOVE    HANTAN(IXA)     TO   HAC-F28.
     MOVE    BIKOU (IXA)     TO   HAC-F29.
     PERFORM    SHO-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         MOVE    SHO-F08     TO   HAC-F30
     ELSE
         MOVE    SPACE       TO   HAC-F30
     END-IF.
     MOVE    BASYO           TO   HAC-F31.
     MOVE    SEIKYU          TO   HAC-F33.
     MOVE    HAKKO           TO   HAC-F34.
     MOVE    1               TO   HAC-F36.
     MOVE    MSGCD           TO   HAC-F39.
 HACHDT-EDIT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.1.2  発注ファイル編集（行＝００）           *
*----------------------------------------------------------*
 HACHDT-EDIT00-SUB      SECTION.
     IF      DENKU = 50 OR 60
         MOVE    ZERO        TO   HA0-F06
     ELSE
         MOVE    1           TO   HA0-F06
     END-IF.
     MOVE    YUBIN           TO   HA0-F07.
     MOVE    OKURI1          TO   HA0-F08.
     MOVE    OKURI2          TO   HA0-F09.
     MOVE    OKURI3          TO   HA0-F10.
     MOVE    TEL             TO   HA0-F14.
 HACHDT-EDIT00-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.1.3  発注ファイル編集（行＝８０）           *
*----------------------------------------------------------*
 HACHDT-EDIT80-SUB      SECTION.
     IF      DENKU = 50 OR 60
         MOVE    ZERO        TO   HA0-F06
     ELSE
         MOVE    1           TO   HA0-F06
     END-IF.
     MOVE    TEKICD          TO   HA0-F11.
     MOVE    TEKI1           TO   HA0-F12.
     MOVE    TEKI2           TO   HA0-F13.
 HACHDT-EDIT80-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.1.4  発注ファイル編集（行＝９９）           *
*----------------------------------------------------------*
 HACHDT-EDIT99-SUB      SECTION.
     IF      DENKU = 50 OR 60
         MOVE    ZERO        TO   HA0-F06
     ELSE
         MOVE    1           TO   HA0-F06
     END-IF.
     MOVE    MSG             TO   WK-MSG.
     MOVE    WK-MSG1         TO   HA0-F08.
     MOVE    WK-MSG2         TO   HA0-F09.
     MOVE    MSGCD           TO   HA0-F15.
 HACHDT-EDIT99-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.2  発注ファイル修正／本日分出力             *
*----------------------------------------------------------*
 HACHDT-UPD-SUB         SECTION.
     INITIALIZE         UPDT-FLG-AREA.
     PERFORM    HAC-START-SUB.
     IF  EOF-FLG  =  ZERO
         PERFORM    HAC-READ-SUB
     END-IF.
     PERFORM  UNTIL (EOF-FLG      =  "1"  )  OR
                    (HAC-F01  NOT = DENKU )  OR
                    (HAC-F02  NOT = HACNO1)  OR
                    (HAC-F03  NOT = HACNO2)
         EVALUATE    HAC-F05
             WHEN    00
                PERFORM   HACHDT-EDIT00-SUB
*## 1999/12/20 NAV
****************COMPUTE HAC-F99  =  SYS-DATE + SEIREKI
                MOVE    SYS-DATE2   TO   HAC-F99
                REWRITE                  HAC-REC
                WRITE  HWK-REC      FROM HAC-REC
                MOVE    "1"         TO   UPDT-FLG00
             WHEN    80
                 PERFORM   HACHDT-EDIT80-SUB
*## 1999/12/20 NAV
*****************COMPUTE HAC-F99  =  SYS-DATE + SEIREKI
                 MOVE    SYS-DATE2   TO   HAC-F99
                 REWRITE                  HAC-REC
                 WRITE  HWK-REC      FROM HAC-REC
                 MOVE    "1"         TO   UPDT-FLG80
             WHEN    99
                 PERFORM   HACHDT-EDIT99-SUB
*## 1999/12/20 NAV
*****************COMPUTE HAC-F99  =  SYS-DATE + SEIREKI
                 MOVE    SYS-DATE2   TO   HAC-F99
                 REWRITE                  HAC-REC
                 WRITE  HWK-REC      FROM HAC-REC
                 MOVE    "1"         TO   UPDT-FLG99
             WHEN    OTHER
                 MOVE  HAC-F05      TO   IXA
                 IF   (SHOCD(IXA)  =    SPACE  )  AND
                      (SURYO(IXA)  NOT  NUMERIC)
                     DELETE             ZHACHDT
                 ELSE
                     PERFORM   HACHDT-EDIT-SUB
*## 1999/12/20 NAV
*********************COMPUTE HAC-F99  =  SYS-DATE + SEIREKI
                     MOVE    SYS-DATE2       TO   HAC-F99
                     REWRITE                      HAC-REC
                     WRITE  HWK-REC          FROM HAC-REC
                     MOVE    "1"             TO   UPDT-FLG(IXA)
                 END-IF
         END-EVALUATE
         PERFORM    HAC-READ-SUB
     END-PERFORM.
*  行＝００の未更新分追加
     IF      UPDT-FLG00      =    SPACE
         IF    ( YUBIN  = SPACE ) AND
               ( OKURI1 = SPACE ) AND
               ( OKURI2 = SPACE ) AND
               ( OKURI3 = SPACE ) AND
               ( TEL    = SPACE )
             CONTINUE
         ELSE
             MOVE    SPACE           TO   HAC0-REC
             INITIALIZE                   HAC0-REC
             MOVE    DENKU           TO   HA0-F01
             MOVE    HACNO1          TO   HA0-F02
             MOVE    HACNO2          TO   HA0-F03
             MOVE    ZERO            TO   HA0-F05
             PERFORM   HACHDT-EDIT00-SUB
*## 1999/12/20 NAV
*************COMPUTE HA0-F98  =  SYS-DATE + SEIREKI
*************COMPUTE HA0-F99  =  SYS-DATE + SEIREKI
             MOVE    SYS-DATE2       TO   HA0-F98
             MOVE    SYS-DATE2       TO   HA0-F99
             WRITE                        HAC-REC
             WRITE   HWK-REC         FROM HAC-REC
         END-IF
     END-IF.
*  行＝０１～０６の未更新分追加
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             IF      UPDT-FLG(IXA)   =    SPACE
                 MOVE    SPACE           TO   HAC1-REC
                 INITIALIZE                   HAC1-REC
                 MOVE    DENKU           TO   HAC-F01
                 MOVE    HACNO1          TO   HAC-F02
                 MOVE    HACNO2          TO   HAC-F03
                 MOVE    IXA             TO   HAC-F05
                 PERFORM   HACHDT-EDIT-SUB
*## 1999/12/20 NAV
*****************COMPUTE HAC-F98  =  SYS-DATE + SEIREKI
*****************COMPUTE HAC-F99  =  SYS-DATE + SEIREKI
                 MOVE    SYS-DATE2       TO   HAC-F98
                 MOVE    SYS-DATE2       TO   HAC-F99
                 WRITE                        HAC-REC
                 WRITE   HWK-REC         FROM HAC-REC
             END-IF
         END-IF
     END-PERFORM.
*  行＝８０，９９の未更新分追加
     IF      UPDT-FLG80      =    SPACE
         IF    ( TEKICD = ZERO  ) AND
               ( TEKI1  = SPACE ) AND
               ( TEKI2  = SPACE )
             CONTINUE
         ELSE
             MOVE    SPACE           TO   HAC0-REC
             INITIALIZE                   HAC0-REC
             MOVE    DENKU           TO   HA0-F01
             MOVE    HACNO1          TO   HA0-F02
             MOVE    HACNO2          TO   HA0-F03
             MOVE    80              TO   HA0-F05
             PERFORM   HACHDT-EDIT80-SUB
*## 1999/12/20 NAV
*************COMPUTE HA0-F98  =  SYS-DATE + SEIREKI
*************COMPUTE HA0-F99  =  SYS-DATE + SEIREKI
             MOVE    SYS-DATE2       TO   HA0-F98
             MOVE    SYS-DATE2       TO   HA0-F99
             WRITE                        HAC-REC
             WRITE   HWK-REC         FROM HAC-REC
         END-IF
     END-IF.
     IF      UPDT-FLG99      =    SPACE
         IF    ( MSGCD  = SPACE ) AND
               ( MSG    = SPACE )
             CONTINUE
         ELSE
             MOVE    SPACE           TO   HAC0-REC
             INITIALIZE                   HAC0-REC
             MOVE    DENKU           TO   HA0-F01
             MOVE    HACNO1          TO   HA0-F02
             MOVE    HACNO2          TO   HA0-F03
             MOVE    99              TO   HA0-F05
             PERFORM   HACHDT-EDIT99-SUB
*## 1999/12/20 NAV
*************COMPUTE HA0-F98  =  SYS-DATE + SEIREKI
*************COMPUTE HA0-F99  =  SYS-DATE + SEIREKI
             MOVE    SYS-DATE2       TO   HA0-F98
             MOVE    SYS-DATE2       TO   HA0-F99
             WRITE                        HAC-REC
             WRITE   HWK-REC         FROM HAC-REC
         END-IF
     END-IF.
 HACHDT-UPD-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.3  発注ファイル削除／本日分出力             *
*----------------------------------------------------------*
 HACHDT-DLT-SUB         SECTION.
     PERFORM    HAC-START-SUB.
     PERFORM    HAC-READ-SUB.
     PERFORM  UNTIL (EOF-FLG      =  "1"  )  OR
                    (HAC-F01  NOT = DENKU )  OR
                    (HAC-F02  NOT = HACNO1)  OR
                    (HAC-F03  NOT = HACNO2)
         EVALUATE    HAC-F05
             WHEN    00
             WHEN    80
             WHEN    99
                 MOVE    1               TO   HA0-F97
             WHEN    OTHER
                 MOVE    1               TO   HAC-F37
         END-EVALUATE
*## 1999/12/20 NAV
*********COMPUTE HAC-F99  =  SYS-DATE + SEIREKI
         MOVE       SYS-DATE2            TO   HAC-F99
         REWRITE                  HAC-REC
         WRITE  HWK-REC      FROM HAC-REC
         PERFORM    HAC-READ-SUB
     END-PERFORM.
 HACHDT-DLT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.4  在庫マスタ更新                           *
*----------------------------------------------------------*
 ZAIMS-UPD-SUB          SECTION.
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             EVALUATE  WK-SYORI
                 WHEN  1
                    MOVE    SURYO(IXA)   TO   WK-SURYO
                 WHEN  2
*\\\ 93.06.01 START \\\
*    修正後の数量を追加する
                    COMPUTE WK-SURYO = SURYO(IXA)
********************COMPUTE WK-SURYO = SURYO(IXA) - OLD-SURYO(IXA)
*\\\ 93.06.01 END   \\\
                 WHEN  3
*\\\
                    IF      OLD-F36(IXA)      = ZERO
                            MOVE ZERO  TO    WK-SURYO
                    ELSE
                            COMPUTE WK-SURYO =
                                    OLD-SURYO(IXA) *  -1
                    END-IF
             END-EVALUATE
             PERFORM ZAI-READ-UPDT
         END-IF
     END-PERFORM.
     IF  WK-SYORI = 2
*\\\ 93.06.01 START \\\

         MOVE        1         TO      TORICD-FLG
*
         PERFORM     VARYING   IXA       FROM    1  BY  1
                     UNTIL     IXA  >  6
*****        IF    DLT-SURYO(IXA)  NOT  =  ZERO
*****            COMPUTE WK-SURYO = DLT-SURYO(IXA) *  -1
*****            MOVE    DLT-SHOCD (IXA)    TO  SHOCD (IXA)
*****            MOVE    DLT-HINTN1(IXA)    TO  HINTN1(IXA)
*****            MOVE    DLT-HINTN2(IXA)    TO  HINTN2(IXA)
*****            MOVE    DLT-HINTN3(IXA)    TO  HINTN3(IXA)
*****            PERFORM ZAI-READ-UPDT
*****        END-IF
*
             IF    OLD-F36  (IXA)       =  1
               IF  SAV-SURYO(IXA)  NOT  =  ZERO
                 COMPUTE WK-SURYO = SAV-SURYO(IXA) *  -1
                 MOVE    SAV-SOKO  (IXA)    TO  BASYO
                 MOVE    SAV-SHOCD (IXA)    TO  SHOCD (IXA)
                 MOVE    SAV-HINTN1(IXA)    TO  HINTN1(IXA)
                 MOVE    SAV-HINTN2(IXA)    TO  HINTN2(IXA)
                 MOVE    SAV-HINTN3(IXA)    TO  HINTN3(IXA)
                 PERFORM ZAI-READ-UPDT
               END-IF
             END-IF
         END-PERFORM
*
         MOVE        ZERO      TO      TORICD-FLG
*\\\ 93.06.01 END   \\\
     END-IF.
 ZAIMS-UPD-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.5.1  在庫マスタ読込／更新                   *
*----------------------------------------------------------*
 ZAI-READ-UPDT          SECTION.
     MOVE    BASYO           TO   ZAI-F01.
     MOVE    SHOCD (IXA)     TO   ZAI-F021.
     MOVE    HINTN1(IXA)     TO   ZAI-F022(1:5).
     MOVE    HINTN2(IXA)     TO   ZAI-F022(6:2).
     MOVE    HINTN3(IXA)     TO   ZAI-F022(8:1).
*\\\ 93.06.01 START \\\
*    TORICD-FLG  =  1 の時変更前の取引先コードで_番を索引
     IF      TORICD-FLG      =     1
             CONTINUE
*************PERFORM         SHO-READ-SUB2
     ELSE
             PERFORM         SHO-READ-SUB
     END-IF.
*****MOVE    ZERO            TO   TORICD-FLG.
*****PERFORM    SHO-READ-SUB.
*\\\ 93.06.01 END   \\\
     IF  INVALID-FLG   =   ZERO
         MOVE    SHO-F08     TO   ZAI-F03    WK-TANA
     ELSE
         MOVE    SPACE       TO   ZAI-F03    WK-TANA
     END-IF.
**** TEST ***
     IF      TORICD-FLG      =     1
             MOVE  SAV-TANA(IXA)   TO  ZAI-F03 WK-TANA
     END-IF.
*******
*****DISPLAY  "XXX = "      ZAI-F03  UPON CONS.
*
**** TEST ***
     READ    ZZAIMS
       INVALID      KEY
          MOVE    SPACE           TO   ZAI-REC
          INITIALIZE                   ZAI-REC
          MOVE    BASYO           TO   ZAI-F01
          MOVE    SHOCD (IXA)     TO   ZAI-F021
          MOVE    HINTN1(IXA)     TO   ZAI-F022(1:5)
          MOVE    HINTN2(IXA)     TO   ZAI-F022(6:2)
          MOVE    HINTN3(IXA)     TO   ZAI-F022(8:1)
          MOVE    WK-TANA         TO   ZAI-F03
          PERFORM    MEI-READ-SUB
          IF  INVALID-FLG   =   ZERO
              MOVE    MEI-F031    TO   ZAI-F04
          ELSE
              MOVE    SPACE       TO   ZAI-F04
          END-IF
          IF  TOKCD     NOT       NUMERIC
              MOVE   ZERO         TO   ZAI-F05
          ELSE
              MOVE   TOKCD        TO   ZAI-F05
          END-IF
**TEST
**********DISPLAY  "WK-SURYO = " WK-SURYO UPON CONS
**********DISPLAY  "$$$$$$$$ = " WK-NOUHIN1 UPON CONS
          EVALUATE    DENKU
              WHEN    50
                IF    KEIJYO     NOT =   1
                      ADD   WK-SURYO  TO     ZAI-F09
                      ADD   WK-SURYO  TO     ZAI-F12
                ELSE
                      ADD       WK-SURYO     TO     ZAI-F06
                      IF    WK-NOUHIN1     >   WK-LSIME
                            ADD       WK-SURYO TO     ZAI-F17
                            ADD       WK-SURYO TO     ZAI-F19
                      ELSE
                            ADD       WK-SURYO TO     ZAI-F10
                            ADD       WK-SURYO TO     ZAI-F08
                      END-IF
                END-IF
              WHEN    51
              WHEN    70
              WHEN    80
*## 1999/12/20 NAV
****************COMPUTE   WK-NOUHIN  =   NOUKI  +   SEIREKI
                MOVE      NEW-NOUKI      TO         WK-NOUHIN
****************DISPLAY   "WK-NOUHIN I = " WK-NOUHIN1 UPON CONS
****************DISPLAY   "WK-SIME   I = " WK-LSIME   UPON CONS
                SUBTRACT  WK-SURYO       FROM   ZAI-F06
                IF    WK-NOUHIN1     >   WK-LSIME
                      SUBTRACT  WK-SURYO       FROM   ZAI-F17
                      SUBTRACT  WK-SURYO       FROM   ZAI-F19
                ELSE
                      SUBTRACT  WK-SURYO       FROM   ZAI-F10
                      SUBTRACT  WK-SURYO       FROM   ZAI-F08
                END-IF
              WHEN    71
              WHEN    81
                ADD       WK-SURYO       TO     ZAI-F06
                ADD       WK-SURYO       TO     ZAI-F08
          END-EVALUATE
****** TEST
****************DISPLAY "ZAI-F09I" ZAI-F09 UPON CONS

          WRITE   ZAI-REC                END-WRITE
       NOT INVALID  KEY
          EVALUATE    DENKU
              WHEN    50
                IF    KEIJYO     NOT =   1
                      ADD   WK-SURYO  TO     ZAI-F09
                      ADD   WK-SURYO  TO     ZAI-F12
                ELSE
                      ADD       WK-SURYO     TO     ZAI-F06
                      IF    WK-NOUHIN1     >   WK-LSIME
                            ADD       WK-SURYO TO     ZAI-F17
                            ADD       WK-SURYO TO     ZAI-F19
                      ELSE
                            ADD       WK-SURYO TO     ZAI-F10
                            ADD       WK-SURYO TO     ZAI-F08
                      END-IF
                END-IF
              WHEN    51
              WHEN    70
              WHEN    80
*## 1999/12/20 NAV
****************COMPUTE   WK-NOUHIN  =   NOUKI  +   SEIREKI
                MOVE      NEW-NOUKI  TO             WK-NOUHIN
****************DISPLAY   "WK-NOUHIN NI= " WK-NOUHIN1 UPON CONS
****************DISPLAY   "WK-SIME   NI= " WK-LSIME   UPON CONS
                SUBTRACT  WK-SURYO       FROM   ZAI-F06
                IF    WK-NOUHIN1     >   WK-LSIME
                      SUBTRACT  WK-SURYO       FROM   ZAI-F17
                      SUBTRACT  WK-SURYO       FROM   ZAI-F19
                ELSE
                      SUBTRACT  WK-SURYO       FROM   ZAI-F10
                      SUBTRACT  WK-SURYO       FROM   ZAI-F08
                END-IF
              WHEN    71
              WHEN    81
                ADD       WK-SURYO       TO     ZAI-F06
                ADD       WK-SURYO       TO     ZAI-F08
          END-EVALUATE
****** TEST
****************DISPLAY "ZAI-F09N" ZAI-F09 UPON CONS

          REWRITE  ZAI-REC
     END-READ.
 ZAI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.5  入庫ファイル登録                         *
*----------------------------------------------------------*
 NYUKDT-OUT-SUB         SECTION.
* TEST
     MOVE    SPACE           TO   NYK-REC.
     INITIALIZE                   NYK-REC.
     MOVE    DENKU           TO   NYK-F01.
     MOVE    HACNO1          TO   NYK-F02.
     MOVE    HACNO2          TO   NYK-F03.
     MOVE    ZERO            TO   NYK-F05.
*## 1999/12/20 NAV
*****COMPUTE NYK-F98  =  SYS-DATE + SEIREKI.
     MOVE    SYS-DATE2       TO   NYK-F98.
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             ADD     1               TO   NYK-F05
             PERFORM   NYUKDT-EDIT-SUB
             WRITE                        NYK-REC
         END-IF
     END-PERFORM.
 NYUKDT-OUT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.5.1  入庫ファイル編集                       *
*----------------------------------------------------------*
 NYUKDT-EDIT-SUB        SECTION.
     MOVE    RYOHAN          TO   NYK-F06.
     MOVE    SIRCD           TO   NYK-F07.
     MOVE    TOKCD           TO   NYK-F08.
     MOVE    ZEIKU           TO   NYK-F09.
     MOVE    SORYOK          TO   NYK-F10.
     MOVE    SORYO           TO   NYK-F11.
     IF  (DENKU = 70 OR 71 OR 80 OR 81) AND
         (TOKCD(1:5) = 99999)
         MOVE    NONYUC      TO   NYK-F12
         MOVE    NONYUC(4:2) TO   NYK-F30
     ELSE
         MOVE    NONYUC      TO   NYK-F12
         MOVE    SPACE       TO   NYK-F30
     END-IF.
     MOVE    SHOCD (IXA)     TO   NYK-F13.
     MOVE    HINTN1(IXA)     TO   NYK-F14(1:5).
     MOVE    HINTN2(IXA)     TO   NYK-F14(6:2).
     MOVE    HINTN3(IXA)     TO   NYK-F14(8:1).
     MOVE    SURYO (IXA)     TO   NYK-F15.
     MOVE    SIRTKB(IXA)     TO   NYK-F16.
     MOVE    SIRTAN(IXA)     TO   NYK-F17.
     MOVE    GENTKB(IXA)     TO   NYK-F18.
     MOVE    GENTAN(IXA)     TO   NYK-F19.
     MOVE    HANTAN(IXA)     TO   NYK-F20.
     MOVE    BIKOU (IXA)     TO   NYK-F21.
     PERFORM    SHO-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         MOVE    SHO-F08     TO   NYK-F22
     ELSE
         MOVE    SPACE       TO   NYK-F22
     END-IF.
*## 1999/12/20 NAV
*****COMPUTE NYK-F23  =  NOUKI  + SEIREKI.
     MOVE    NEW-NOUKI       TO   NYK-F23.
     MOVE    BASYO           TO   NYK-F24.
     MOVE    SEIKYU          TO   NYK-F25.
     MOVE    HAKKO           TO   NYK-F26.
     MOVE    1               TO   NYK-F27.
*## 1999/12/20 NAV
*****COMPUTE NYK-F29  =  HACYMD + SEIREKI.
     MOVE    NEW-HACYMD      TO   NYK-F29.
 NYUKDT-EDIT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.6  入庫ファイル修正                         *
*----------------------------------------------------------*
 NYUKDT-UPD-SUB         SECTION.
     INITIALIZE         UPDT-FLG-AREA.
     PERFORM    NYK-START-SUB.
     PERFORM    NYK-READ-SUB.
     PERFORM  UNTIL  EOF-FLG  =  "1"
         MOVE  NYK-F05          TO   IXA
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             DELETE             ZNYUKDT
         ELSE
             PERFORM   NYUKDT-EDIT-SUB
*## 1999/12/20 NAV
*************COMPUTE NYK-F99  =  SYS-DATE + SEIREKI
             MOVE      SYS-DATE2     TO   NYK-F99
             REWRITE                      NYK-REC
             MOVE    "1"             TO   UPDT-FLG(IXA)
         END-IF
         PERFORM    NYK-READ-SUB
     END-PERFORM.
*  未更新分追加
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             IF      UPDT-FLG(IXA)   =    SPACE
                 MOVE    SPACE           TO   NYK-REC
                 INITIALIZE                   NYK-REC
                 MOVE    DENKU           TO   NYK-F01
                 MOVE    HACNO1          TO   NYK-F02
                 MOVE    HACNO2          TO   NYK-F03
                 MOVE    IXA             TO   NYK-F05
                 PERFORM   NYUKDT-EDIT-SUB
*## 1999/12/20 NAV
*****************COMPUTE NYK-F98  =  SYS-DATE + SEIREKI
*****************COMPUTE NYK-F99  =  SYS-DATE + SEIREKI
                 MOVE    SYS-DATE2       TO   NYK-F98
                 MOVE    SYS-DATE2       TO   NYK-F99
                 WRITE                        NYK-REC
             END-IF
         END-IF
     END-PERFORM.
 NYUKDT-UPD-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.7  入庫ファイル削除                         *
*----------------------------------------------------------*
 NYUKDT-DLT-SUB         SECTION.
     PERFORM    NYK-START-SUB.
     PERFORM    NYK-READ-SUB.
     PERFORM  UNTIL  EOF-FLG  =  "1"
         MOVE  1                TO   NYK-F97
*## 1999/12/20 NAV
*********COMPUTE NYK-F99  =  SYS-DATE + SEIREKI
         MOVE       SYS-DATE2   TO   NYK-F99
         REWRITE    NYK-REC
         PERFORM    NYK-READ-SUB
     END-PERFORM.
 NYUKDT-DLT-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      HJYOKEN   HTOKMS    HTENMS
              ZSHIMS    ZSOKMS    HMEIMS    HSHOTBL
              ZZAIMS    ZHACHDT   ZNYUKDT   ZHACHWK.
 END-END.
     EXIT.
************************************************************
*      9.0        共通処理                                 *
************************************************************
*==========================================================*
*      9.1       エラー メッセージセット                   *
*==========================================================*
 MSG-SEC                SECTION.
     IF  ERR-MSG-CD     =    ZERO
         MOVE    SPACE       TO   ERRMSG
*93.10.21
       IF        MAIN-FLG =       4 OR 5
         IF      DENKU  =    50   AND
                 KEIJYO =    1
                 MOVE NC"確認で計上されます"
                        TO        ERRMSG
         END-IF
       END-IF
     ELSE
         MOVE    ERR-MSG(ERR-MSG-CD)  TO   ERRMSG
         MOVE    ZERO        TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*==========================================================*
*      9.2       項目制御部初期化                          *
*==========================================================*
*----------------------------------------------------------*
*      9.2.1     項目制御部初期化（処理区分）              *
*----------------------------------------------------------*
 EDIT-SET-SYORI         SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  SYORIK.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SYORIK.
 EDIT-SET-SYORI-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.2     項目制御部初期化（ヘッド部）              *
*----------------------------------------------------------*
 EDIT-SET-HEAD          SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  DENKU.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DENKU.
     MOVE   "M"     TO   EDIT-OPTION  OF  KEIJYO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KEIJYO.
     MOVE   "M"     TO   EDIT-OPTION  OF  HACNO1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HACNO1.
     MOVE   "M"     TO   EDIT-OPTION  OF  HACNO2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HACNO2.
     MOVE   "M"     TO   EDIT-OPTION  OF  HACYMD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HACYMD.
     MOVE   "M"     TO   EDIT-OPTION  OF  NOUKI.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  NOUKI.
     MOVE   "M"     TO   EDIT-OPTION  OF  TANTO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANTO.
     MOVE   "M"     TO   EDIT-OPTION  OF  BASYO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  BASYO.
     MOVE   "M"     TO   EDIT-OPTION  OF  RYOHAN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  RYOHAN.
     MOVE   "M"     TO   EDIT-OPTION  OF  SORYOK.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SORYOK.
     MOVE   "M"     TO   EDIT-OPTION  OF  SORYO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SORYO.
     MOVE   "M"     TO   EDIT-OPTION  OF  SIRCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIRCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  SEIKYU.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SEIKYU.
     MOVE   "M"     TO   EDIT-OPTION  OF  HAKKO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HAKKO.
     MOVE   "M"     TO   EDIT-OPTION  OF  ZEIKU.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  ZEIKU.
     MOVE   "M"     TO   EDIT-OPTION  OF  MEMO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MEMO.
     MOVE   "M"     TO   EDIT-OPTION  OF  TOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TOKCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  NONYUC.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  NONYUC.
     MOVE   "M"     TO   EDIT-OPTION  OF  SOFUKB.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SOFUKB.
     MOVE   "M"     TO   EDIT-OPTION  OF  YUBIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  YUBIN.
     MOVE   "M"     TO   EDIT-OPTION  OF  OKURI1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  OKURI1.
     MOVE   "M"     TO   EDIT-OPTION  OF  TEL.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TEL.
     MOVE   "M"     TO   EDIT-OPTION  OF  OKURI2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  OKURI2.
     MOVE   "M"     TO   EDIT-OPTION  OF  OKURI3.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  OKURI3.
     MOVE   "M"     TO   EDIT-OPTION  OF  MSGCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MSGCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  MSG.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MSG.
 EDIT-SET-HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3     項目制御部初期化（ボディ部）              *
*----------------------------------------------------------*
 EDIT-SET-BODY          SECTION.
     PERFORM  VARYING IXB     FROM  1  BY  1
              UNTIL   IXB     >     6
         PERFORM     EDIT-SET-GYO
     END-PERFORM.
     MOVE   "M"     TO   EDIT-OPTION  OF  TEKICD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TEKICD.
     MOVE   "M"     TO   EDIT-OPTION  OF  TEKI1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TEKI1.
     MOVE   "M"     TO   EDIT-OPTION  OF  TEKI2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TEKI2.
     MOVE   "M"     TO   EDIT-OPTION  OF  ANS.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  ANS.
 EDIT-SET-BODY-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3.1   項目制御部初期化（ボディ部１行）          *
*----------------------------------------------------------*
 EDIT-SET-GYO           SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  SHOCD (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SHOCD (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HINTN1(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HINTN1(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HINTN2(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HINTN2(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HINTN3(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HINTN3(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SURYO (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SURYO (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SIRTKB(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIRTKB(IXB).
     IF    CHK-SOK   =   87
         MOVE   "R"      TO   EDIT-OPTION  OF  SIRTAN(IXB)
     ELSE
         MOVE   "M"      TO   EDIT-OPTION  OF  SIRTAN(IXB)
     END-IF.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIRTAN(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  GENTKB(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  GENTKB(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  GENTAN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  GENTAN(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HANTAN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HANTAN(IXB).
 EDIT-SET-GYO-END.
     EXIT.
*==========================================================*
*      9.3       画面表示処理                              *
*==========================================================*
 DSP-WRITE-SUB          SECTION.
     MOVE    "ZHA0040 "      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     WRITE    ZHA0040.
 DSP-WRITE-END.
     EXIT.
*==========================================================*
*      9.4       画面データの入力処理                      *
*==========================================================*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*==========================================================*
*      9.5       ＨＥＡＤ部消去                            *
*==========================================================*
 HEADDEL-SUB            SECTION.
     MOVE   SPACE       TO   HEAD0.
     PERFORM            EDIT-SET-HEAD.
 HEADDEL-END.
     EXIT.
*==========================================================*
*      9.6       ＢＯＤＹ部消去                            *
*==========================================================*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   BODY0.
     PERFORM            EDIT-SET-BODY.
 BODYDEL-END.
     EXIT.
*==========================================================*
*      9.7       ファイル処理                              *
*==========================================================*
*----------------------------------------------------------*
*      9.7.1     条件ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 JYO-READ-SUB           SECTION.
     READ    HJYOKEN
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
          INITIALIZE              JYO-REC
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
 JYO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2.1   発注ファイルＳＴＡＲＴ                    *
*----------------------------------------------------------*
 HAC-START-SUB          SECTION.
     MOVE    DENKU           TO   HAC-F01.
     MOVE    HACNO1          TO   HAC-F02.
     MOVE    HACNO2          TO   HAC-F03.
     MOVE    ZERO            TO   HAC-F04.
     MOVE    ZERO            TO   HAC-F05.
     START   ZHACHDT    KEY  >=   HAC-F01
                                  HAC-F02
                                  HAC-F03
                                  HAC-F04
                                  HAC-F05
       INVALID      KEY
          MOVE     "1"       TO   EOF-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   EOF-FLG
     END-START.
 HAC-START-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2.2   発注ファイルＢＡＣＫＳＴＡＲＴ            *
*----------------------------------------------------------*
 HAC-BACKSTART-SUB      SECTION.
     MOVE    SV-DENKU        TO   HAC-F01.
     MOVE    SV-HACNO1       TO   HAC-F02.
     MOVE    SV-HACNO2       TO   HAC-F03.
     MOVE    ZERO            TO   HAC-F04.
     MOVE    ZERO            TO   HAC-F05.
     START   ZHACHDT    KEY  <    HAC-F01
                                  HAC-F02
                                  HAC-F03
                                  HAC-F04
                                  HAC-F05
                                  REVERSED
       INVALID      KEY
          MOVE     "1"       TO   EOF-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   EOF-FLG
     END-START.
 HAC-BACKSTART-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2.3   発注ファイルＮＥＸＴＳＴＡＲＴ            *
*----------------------------------------------------------*
 HAC-NEXTSTART-SUB      SECTION.
     MOVE    SV-DENKU        TO   HAC-F01.
     MOVE    SV-HACNO1       TO   HAC-F02.
     MOVE    SV-HACNO2       TO   HAC-F03.
     MOVE    9               TO   HAC-F04.
     MOVE    99              TO   HAC-F05.
     START   ZHACHDT    KEY  >    HAC-F01
                                  HAC-F02
                                  HAC-F03
                                  HAC-F04
                                  HAC-F05
       INVALID      KEY
          MOVE     "1"       TO   EOF-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   EOF-FLG
     END-START.
 HAC-NEXTSTART-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.3.1   発注ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 HAC-READ-SUB           SECTION.
     READ    ZHACHDT    NEXT
       AT  END
          MOVE     "1"       TO   EOF-FLG
       NOT AT  END
          IF  (WK-SYORI = 2  OR  3 )  AND
              (HAC-F01  = SV-DENKU )  AND
              (HAC-F02  = SV-HACNO1)  AND
              (HAC-F03  = SV-HACNO2)
              IF  (HAC-F05  >=  1)  AND  (HAC-F05  <= 6)
                  IF   HAC-F23  NOT = ZERO
                       MOVE     "1"        TO   READ-FLG
                       GO        TO   HAC-READ-SUB
                  END-IF
                  IF   HAC-F37  NOT = ZERO
                       MOVE     "1"        TO   READ-FLG
                       GO        TO   HAC-READ-SUB
                  END-IF
                  IF   HAC-F01 = 50 OR 60
                      IF   HAC-F06  NOT = ZERO
                           MOVE     "1"        TO   READ-FLG
                           GO        TO   HAC-READ-SUB
                      END-IF
                  ELSE
                      IF   HAC-F35  NOT = ZERO
                           MOVE     "1"        TO   READ-FLG
                           GO        TO   HAC-READ-SUB
                      END-IF
                  END-IF
              ELSE
                  IF   HA0-F01 = 50 OR 60
                      IF   HA0-F06  NOT = ZERO
                           MOVE     "1"        TO   READ-FLG
                           GO        TO   HAC-READ-SUB
                      END-IF
                  END-IF
                  IF   HA0-F97  NOT = ZERO
                       MOVE     "1"        TO   READ-FLG
                       GO        TO   HAC-READ-SUB
                  END-IF
              END-IF
          END-IF
          MOVE     ZERO      TO   EOF-FLG
     END-READ.
 HAC-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.3.2   発注ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 HAC-CHK-SUB            SECTION.
     IF   WK-SYORI = 2  OR  3
         IF  (HAC-F05  >=  1)  AND  (HAC-F05  <= 6)
             IF   HAC-F23  NOT = ZERO
                  MOVE     "1"        TO   READ-FLG
             END-IF
             IF   HAC-F37  NOT = ZERO
                  MOVE     "1"        TO   READ-FLG
             END-IF
             IF   HAC-F01 = 50 OR 60
                 IF   HAC-F06  NOT = ZERO
                      MOVE     "1"        TO   READ-FLG
                 END-IF
             ELSE
                 IF   HAC-F35  NOT = ZERO
                      MOVE     "1"        TO   READ-FLG
                 END-IF
             END-IF
         ELSE
             IF   HA0-F01 = 50 OR 60
                 IF   HA0-F06  NOT = ZERO
                      MOVE     "1"        TO   READ-FLG
                 END-IF
             END-IF
             IF   HA0-F97  NOT = ZERO
                  MOVE     "1"        TO   READ-FLG
             END-IF
         END-IF
     END-IF.
 HAC-CHK-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.4     場所名の取得                              *
*----------------------------------------------------------*
 BSY-READ-SUB           SECTION.
     MOVE    BASYO           TO   SOK-F01.
     READ    ZSOKMS
       INVALID      KEY
          MOVE     SPACE     TO   BASYOM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     SOK-F02   TO   BASYOM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 BSY-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.5     納入先名の取得                            *
*----------------------------------------------------------*
 NYS-READ-SUB           SECTION.
     MOVE    NONYUC(4:2)     TO   SOK-F01.
     READ    ZSOKMS
       INVALID      KEY
          MOVE     SPACE     TO   NONYUM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     SOK-F02   TO   NONYUM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 NYS-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.6     仕入先名の取得                            *
*----------------------------------------------------------*
 SHI-READ-SUB           SECTION.
     MOVE    SIRCD           TO   SHI-F01.
     READ    ZSHIMS
       INVALID      KEY
          MOVE     SPACE     TO   SIRNM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     SHI-F02   TO   SIRNM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.8     得意先名の取得                            *
*----------------------------------------------------------*
 TOK-READ-SUB           SECTION.
     MOVE    TOKCD           TO   TOK-F01.
     READ    HTOKMS
       INVALID      KEY
          MOVE     SPACE     TO   TOKNM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     TOK-F02   TO   TOKNM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 TOK-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.9     店舗名の取得                              *
*----------------------------------------------------------*
 TEN-READ-SUB           SECTION.
     MOVE    TOKCD           TO   TEN-F52.
     MOVE    NONYUC          TO   TEN-F011.
     READ    HTENMS
       INVALID      KEY
          MOVE     SPACE     TO   NONYUM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     TEN-F02   TO   NONYUM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 TEN-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.10    商品名の取得                              *
*----------------------------------------------------------*
 MEI-READ-SUB           SECTION.
     MOVE    SHOCD (IXA)     TO   MEI-F01.
     MOVE    HINTN1(IXA)     TO   MEI-F0121.
     MOVE    HINTN2(IXA)     TO   MEI-F0122.
     MOVE    HINTN3(IXA)     TO   MEI-F0123.
     READ    HMEIMS
       INVALID      KEY
          MOVE     SPACE     TO   HINMEI(IXA)
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     MEI-F02   TO   HINMEI(IXA)
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 MEI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.10    商品変換テーブル読込                      *
*----------------------------------------------------------*
 SHO-READ-SUB           SECTION.
     MOVE    TOKCD           TO   SHO-F01.
     MOVE    SHOCD (IXA)     TO   SHO-F031.
     MOVE    HINTN1(IXA)     TO   SHO-F0321.
     MOVE    HINTN2(IXA)     TO   SHO-F0322.
     MOVE    HINTN3(IXA)     TO   SHO-F0323.
     READ    HSHOTBL
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.10.2  商品変換テーブル読込（２）                *
*----------------------------------------------------------*
 SHO-READ-SUB2          SECTION.
     MOVE    SAV-TORICD(1)   TO   SHO-F01.
     MOVE    SHOCD (IXA)     TO   SHO-F031.
     MOVE    HINTN1(IXA)     TO   SHO-F0321.
     MOVE    HINTN2(IXA)     TO   SHO-F0322.
     MOVE    HINTN3(IXA)     TO   SHO-F0323.
     READ    HSHOTBL
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.11    入庫ファイルＳＴＡＲＴ                    *
*----------------------------------------------------------*
 NYK-START-SUB          SECTION.
     MOVE    DENKU           TO   NYK-F01.
     MOVE    HACNO1          TO   NYK-F02.
     MOVE    HACNO2          TO   NYK-F03.
     MOVE    ZERO            TO   NYK-F04.
     MOVE    ZERO            TO   NYK-F05.
     START   ZNYUKDT    KEY  >=   NYK-F01
                                  NYK-F02
                                  NYK-F03
                                  NYK-F04
                                  NYK-F05
       INVALID      KEY
          MOVE     "1"       TO   EOF-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   EOF-FLG
     END-START.
 NYK-START-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.12    入庫ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 NYK-READ-SUB           SECTION.
     READ    ZNYUKDT    NEXT
       AT  END
          MOVE     "1"       TO   EOF-FLG
       NOT AT  END
          IF  (NYK-F01  =  DENKU )  AND
              (NYK-F02  =  HACNO1)  AND
              (NYK-F03  =  HACNO2)
              MOVE      ZERO      TO   EOF-FLG
          ELSE
              MOVE      "1"       TO   EOF-FLG
          END-IF
     END-READ.
 NYK-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.8       日付の論理チェック                        *
*----------------------------------------------------------*
 YMDCHK-SUB             SECTION.
     MOVE    ZERO                TO    INVALID-FLG.
***閏年判定
*****PERFORM URUCHK-SUB.
***日付論理チェック
*****IF    ( CHK-MM              <     01 )  OR
*****      ( CHK-MM              >     12 )  OR
*****      ( CHK-DD              <     01 )  OR
*****      ( CHK-DD              >     31 )
*****    MOVE    "1"             TO   INVALID-FLG
*****ELSE
*****    IF      CHK-DD          >     DAY-T(CHK-MM)
*****        MOVE    "1"         TO   INVALID-FLG
*****    END-IF
*****END-IF.
*## 1999/12/20 NAV 日付判定方法変更サブルーチン使用
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     CHK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET NOT = ZERO
              MOVE    "1"         TO   INVALID-FLG
     ELSE
              MOVE     ZERO       TO   INVALID-FLG
     END-IF.
*
 YMDCHK-END.
     EXIT.
*----------------------------------------------------------*
*      9.8       閏年の判定                                *
*----------------------------------------------------------*
 URUCHK-SUB             SECTION.
     COMPUTE CHK-YMD     =       CHK-YMD   +   SEIREKI.
     DIVIDE  CHK-YY              BY    4
                         GIVING  CHK-SHO   REMAINDER   CHK-R4
     DIVIDE  CHK-YY              BY    100
                         GIVING  CHK-SHO   REMAINDER   CHK-R100
     DIVIDE  CHK-YY              BY    400
                         GIVING  CHK-SHO   REMAINDER   CHK-R400
     IF  ( ( CHK-R4              =     ZERO )    AND
           ( CHK-R100        NOT =     ZERO ) )  OR
         (   CHK-R400            =     ZERO   )
         MOVE  29                TO    DAY-T(2)
     ELSE
         MOVE  28                TO    DAY-T(2)
     END-IF.
 URUCHK-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
